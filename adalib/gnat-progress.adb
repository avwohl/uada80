-- GNAT.Progress body for Z80
-- Progress indicator implementation

package body GNAT.Progress is

   function Int_Image (V : Integer) return String is
      S : constant String := Integer'Image (V);
   begin
      if V >= 0 then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end Int_Image;

   ------------------
   -- Progress_Bar --
   ------------------

   procedure Initialize (PB : out Progress_Bar;
                         Min_Value : Integer := 0;
                         Max_Value : Integer := 100;
                         Width : Positive := 20) is
   begin
      PB.Min_Val := Min_Value;
      PB.Max_Val := Max_Value;
      PB.Current := Min_Value;
      PB.Width := Width;
      if PB.Width > Max_Bar_Width then
         PB.Width := Max_Bar_Width;
      end if;
      PB.Style := Hash_Style;
      PB.Filled_C := '#';
      PB.Empty_C := '-';
      PB.Label := (others => ' ');
      PB.Label_Len := 0;
   end Initialize;

   procedure Set_Value (PB : in Out Progress_Bar; Value : Integer) is
   begin
      PB.Current := Value;
      if PB.Current < PB.Min_Val then
         PB.Current := PB.Min_Val;
      elsif PB.Current > PB.Max_Val then
         PB.Current := PB.Max_Val;
      end if;
   end Set_Value;

   procedure Increment (PB : in Out Progress_Bar; Amount : Positive := 1) is
   begin
      Set_Value (PB, PB.Current + Amount);
   end Increment;

   procedure Set_Label (PB : in Out Progress_Bar; Label : String) is
   begin
      PB.Label := (others => ' ');
      PB.Label_Len := Label'Length;
      if PB.Label_Len > Max_Label_Len then
         PB.Label_Len := Max_Label_Len;
      end if;
      for I in 1 .. PB.Label_Len loop
         PB.Label (I) := Label (Label'First + I - 1);
      end loop;
   end Set_Label;

   function Get_Percent (PB : Progress_Bar) return Natural is
      Range_Val : constant Integer := PB.Max_Val - PB.Min_Val;
   begin
      if Range_Val <= 0 then
         return 0;
      end if;
      return ((PB.Current - PB.Min_Val) * 100) / Range_Val;
   end Get_Percent;

   function Get_Value (PB : Progress_Bar) return Integer is
   begin
      return PB.Current;
   end Get_Value;

   function Is_Complete (PB : Progress_Bar) return Boolean is
   begin
      return PB.Current >= PB.Max_Val;
   end Is_Complete;

   procedure Set_Style (PB : in Out Progress_Bar; Style : Bar_Style) is
   begin
      PB.Style := Style;
      case Style is
         when Hash_Style =>
            PB.Filled_C := '#';
            PB.Empty_C := '-';
         when Block_Style =>
            PB.Filled_C := '=';
            PB.Empty_C := ' ';
         when Arrow_Style =>
            PB.Filled_C := '=';
            PB.Empty_C := '-';
         when Dots_Style =>
            PB.Filled_C := '.';
            PB.Empty_C := ' ';
      end case;
   end Set_Style;

   procedure Set_Chars (PB : in Out Progress_Bar;
                        Filled : Character;
                        Empty : Character) is
   begin
      PB.Filled_C := Filled;
      PB.Empty_C := Empty;
   end Set_Chars;

   function Render (PB : Progress_Bar) return String is
      Result : String (1 .. PB.Width + 2);
      Filled : constant Natural :=
        (Get_Percent (PB) * PB.Width) / 100;
   begin
      Result (1) := '[';

      for I in 2 .. PB.Width + 1 loop
         if I - 1 <= Filled then
            Result (I) := PB.Filled_C;
         else
            Result (I) := PB.Empty_C;
         end if;
      end loop;

      -- Arrow head for arrow style
      if PB.Style = Arrow_Style and Filled > 0 and Filled < PB.Width then
         Result (Filled + 1) := '>';
      end if;

      Result (PB.Width + 2) := ']';
      return Result;
   end Render;

   function Render_With_Percent (PB : Progress_Bar) return String is
      Pct : constant Natural := Get_Percent (PB);
      Pct_Str : constant String := Int_Image (Pct);
   begin
      return Render (PB) & " " & Pct_Str & "%";
   end Render_With_Percent;

   function Render_Full (PB : Progress_Bar) return String is
   begin
      if PB.Label_Len > 0 then
         return PB.Label (1 .. PB.Label_Len) & ": " & Render_With_Percent (PB);
      else
         return Render_With_Percent (PB);
      end if;
   end Render_Full;

   -------------
   -- Spinner --
   -------------

   procedure Initialize (S : out Spinner) is
   begin
      S.Frame := 0;
      S.Style := Line_Style;
   end Initialize;

   procedure Tick (S : in Out Spinner) is
   begin
      S.Frame := S.Frame + 1;
      if S.Frame > 3 then
         S.Frame := 0;
      end if;
   end Tick;

   function Current_Char (S : Spinner) return Character is
   begin
      case S.Style is
         when Line_Style =>
            case S.Frame is
               when 0 => return '|';
               when 1 => return '/';
               when 2 => return '-';
               when others => return '\';
            end case;
         when Circle_Style =>
            case S.Frame is
               when 0 => return '-';
               when 1 => return '\';
               when 2 => return '|';
               when others => return '/';
            end case;
         when Dots_Style =>
            case S.Frame is
               when 0 => return '.';
               when 1 => return 'o';
               when 2 => return 'O';
               when others => return 'o';
            end case;
         when Arrow_Style =>
            case S.Frame is
               when 0 => return '^';
               when 1 => return '>';
               when 2 => return 'v';
               when others => return '<';
            end case;
      end case;
   end Current_Char;

   function Render (S : Spinner; Label : String := "") return String is
   begin
      if Label'Length > 0 then
         return Current_Char (S) & " " & Label;
      else
         return (1 => Current_Char (S));
      end if;
   end Render;

   procedure Set_Style (S : in Out Spinner; Style : Spinner_Style) is
   begin
      S.Style := Style;
   end Set_Style;

   -------------
   -- Counter --
   -------------

   procedure Initialize (C : out Counter;
                         Total : Positive;
                         Prefix : String := "Item ";
                         Suffix : String := "") is
   begin
      C.Current := 1;
      C.Total := Total;

      C.Prefix := (others => ' ');
      C.Prefix_Len := Prefix'Length;
      if C.Prefix_Len > Max_Label_Len then
         C.Prefix_Len := Max_Label_Len;
      end if;
      for I in 1 .. C.Prefix_Len loop
         C.Prefix (I) := Prefix (Prefix'First + I - 1);
      end loop;

      C.Suffix := (others => ' ');
      C.Suffix_Len := Suffix'Length;
      if C.Suffix_Len > Max_Label_Len then
         C.Suffix_Len := Max_Label_Len;
      end if;
      for I in 1 .. C.Suffix_Len loop
         C.Suffix (I) := Suffix (Suffix'First + I - 1);
      end loop;
   end Initialize;

   procedure Set_Count (C : in Out Counter; Value : Positive) is
   begin
      C.Current := Value;
      if C.Current > C.Total then
         C.Current := C.Total;
      end if;
   end Set_Count;

   procedure Increment (C : in Out Counter) is
   begin
      if C.Current < C.Total then
         C.Current := C.Current + 1;
      end if;
   end Increment;

   function Get_Count (C : Counter) return Positive is
   begin
      return C.Current;
   end Get_Count;

   function Get_Total (C : Counter) return Positive is
   begin
      return C.Total;
   end Get_Total;

   function Is_Complete (C : Counter) return Boolean is
   begin
      return C.Current >= C.Total;
   end Is_Complete;

   function Render (C : Counter) return String is
   begin
      return C.Prefix (1 .. C.Prefix_Len) &
             Int_Image (C.Current) & " of " & Int_Image (C.Total) &
             C.Suffix (1 .. C.Suffix_Len);
   end Render;

   function Render_With_Percent (C : Counter) return String is
      Pct : constant Natural := (C.Current * 100) / C.Total;
   begin
      return Render (C) & " (" & Int_Image (Pct) & "%)";
   end Render_With_Percent;

   ---------------
   -- Utilities --
   ---------------

   function Percent_Bar (Percent : Natural; Width : Positive := 20) return String is
      Result : String (1 .. Width + 2);
      Filled : constant Natural := (Percent * Width) / 100;
   begin
      Result (1) := '[';
      for I in 2 .. Width + 1 loop
         if I - 1 <= Filled then
            Result (I) := '#';
         else
            Result (I) := '-';
         end if;
      end loop;
      Result (Width + 2) := ']';
      return Result;
   end Percent_Bar;

   function Ratio_Display (Current, Total : Natural) return String is
   begin
      return Int_Image (Current) & "/" & Int_Image (Total);
   end Ratio_Display;

   function Elapsed_Display (Seconds : Natural) return String is
      H : constant Natural := Seconds / 3600;
      M : constant Natural := (Seconds / 60) mod 60;
      S : constant Natural := Seconds mod 60;

      function Pad (N : Natural) return String is
         Str : constant String := Int_Image (N);
      begin
         if N < 10 then
            return "0" & Str;
         else
            return Str;
         end if;
      end Pad;
   begin
      if H > 0 then
         return Pad (H) & ":" & Pad (M) & ":" & Pad (S);
      else
         return Pad (M) & ":" & Pad (S);
      end if;
   end Elapsed_Display;

   function ETA_Display (Percent : Natural; Elapsed_Seconds : Natural) return String is
      Remaining : Natural;
   begin
      if Percent = 0 or Percent >= 100 then
         return "--:--";
      end if;

      -- ETA = Elapsed * (100 - Percent) / Percent
      Remaining := (Elapsed_Seconds * (100 - Percent)) / Percent;
      return Elapsed_Display (Remaining);
   end ETA_Display;

end GNAT.Progress;
