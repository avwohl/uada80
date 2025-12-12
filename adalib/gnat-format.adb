-- GNAT.Format body for Z80
-- String formatting implementation

package body GNAT.Format is

   Hex_Digits : constant String := "0123456789ABCDEF";

   ---------------
   -- Int_Image --
   ---------------

   function Int_Image (Value : Integer) return String is
      S : constant String := Integer'Image (Value);
   begin
      if Value >= 0 then
         return S (S'First + 1 .. S'Last);  -- Remove leading space
      else
         return S;  -- Keep minus sign
      end if;
   end Int_Image;

   function Int_Image (Value : Integer; Width : Positive) return String is
   begin
      return Pad_Left (Int_Image (Value), Width);
   end Int_Image;

   ----------------------
   -- Int_Image_Padded --
   ----------------------

   function Int_Image_Padded (Value : Integer; Width : Positive;
                              Pad : Character := '0') return String is
      S : constant String := Int_Image (abs Value);
   begin
      if Value < 0 then
         return "-" & Pad_Left (S, Width - 1, Pad);
      else
         return Pad_Left (S, Width, Pad);
      end if;
   end Int_Image_Padded;

   --------------------
   -- Unsigned_Image --
   --------------------

   function Unsigned_Image (Value : Natural) return String is
   begin
      return Int_Image (Value);
   end Unsigned_Image;

   function Unsigned_Image (Value : Natural; Base : Positive) return String is
      Result : String (1 .. 16);
      Pos    : Natural := Result'Last;
      V      : Natural := Value;
      D      : Natural;
   begin
      if V = 0 then
         return "0";
      end if;

      while V > 0 loop
         D := V mod Base;
         if D < 10 then
            Result (Pos) := Character'Val (Character'Pos ('0') + D);
         else
            Result (Pos) := Character'Val (Character'Pos ('A') + D - 10);
         end if;
         Pos := Pos - 1;
         V := V / Base;
      end loop;

      return Result (Pos + 1 .. Result'Last);
   end Unsigned_Image;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image (Value : Natural) return String is
   begin
      return Unsigned_Image (Value, 16);
   end Hex_Image;

   function Hex_Image (Value : Natural; Width : Positive) return String is
   begin
      return Pad_Left (Hex_Image (Value), Width, '0');
   end Hex_Image;

   ------------------
   -- Binary_Image --
   ------------------

   function Binary_Image (Value : Natural) return String is
   begin
      return Unsigned_Image (Value, 2);
   end Binary_Image;

   function Binary_Image (Value : Natural; Width : Positive) return String is
   begin
      return Pad_Left (Binary_Image (Value), Width, '0');
   end Binary_Image;

   -----------------
   -- Octal_Image --
   -----------------

   function Octal_Image (Value : Natural) return String is
   begin
      return Unsigned_Image (Value, 8);
   end Octal_Image;

   ----------------
   -- Bool_Image --
   ----------------

   function Bool_Image (Value : Boolean) return String is
   begin
      if Value then
         return "TRUE";
      else
         return "FALSE";
      end if;
   end Bool_Image;

   -----------------
   -- Bool_Yes_No --
   -----------------

   function Bool_Yes_No (Value : Boolean) return String is
   begin
      if Value then
         return "Yes";
      else
         return "No";
      end if;
   end Bool_Yes_No;

   -----------------
   -- Bool_On_Off --
   -----------------

   function Bool_On_Off (Value : Boolean) return String is
   begin
      if Value then
         return "On";
      else
         return "Off";
      end if;
   end Bool_On_Off;

   --------------
   -- Pad_Left --
   --------------

   function Pad_Left (S : String; Width : Positive;
                      Pad : Character := ' ') return String is
   begin
      if S'Length >= Width then
         return S;
      end if;

      declare
         Result : String (1 .. Width) := (others => Pad);
         Start  : constant Positive := Width - S'Length + 1;
      begin
         Result (Start .. Width) := S;
         return Result;
      end;
   end Pad_Left;

   ---------------
   -- Pad_Right --
   ---------------

   function Pad_Right (S : String; Width : Positive;
                       Pad : Character := ' ') return String is
   begin
      if S'Length >= Width then
         return S;
      end if;

      declare
         Result : String (1 .. Width) := (others => Pad);
      begin
         Result (1 .. S'Length) := S;
         return Result;
      end;
   end Pad_Right;

   ------------
   -- Center --
   ------------

   function Center (S : String; Width : Positive;
                    Pad : Character := ' ') return String is
      Left_Pad : Natural;
   begin
      if S'Length >= Width then
         return S;
      end if;

      Left_Pad := (Width - S'Length) / 2;

      declare
         Result : String (1 .. Width) := (others => Pad);
      begin
         Result (Left_Pad + 1 .. Left_Pad + S'Length) := S;
         return Result;
      end;
   end Center;

   -------------------
   -- Truncate_Left --
   -------------------

   function Truncate_Left (S : String; Max_Width : Positive) return String is
   begin
      if S'Length <= Max_Width then
         return S;
      end if;
      return S (S'Last - Max_Width + 1 .. S'Last);
   end Truncate_Left;

   --------------------
   -- Truncate_Right --
   --------------------

   function Truncate_Right (S : String; Max_Width : Positive) return String is
   begin
      if S'Length <= Max_Width then
         return S;
      end if;
      return S (S'First .. S'First + Max_Width - 1);
   end Truncate_Right;

   ---------------------
   -- Truncate_Middle --
   ---------------------

   function Truncate_Middle (S : String; Max_Width : Positive) return String is
      Half : Natural;
   begin
      if S'Length <= Max_Width then
         return S;
      end if;

      if Max_Width < 5 then
         return Truncate_Right (S, Max_Width);
      end if;

      Half := (Max_Width - 3) / 2;
      return S (S'First .. S'First + Half - 1) & "..." &
             S (S'Last - Half + 1 .. S'Last);
   end Truncate_Middle;

   -----------------
   -- Fixed_Image --
   -----------------

   function Fixed_Image (Value : Integer) return String is
   begin
      return Fixed_Image (Value, 2);
   end Fixed_Image;

   function Fixed_Image (Value : Integer; Decimals : Natural) return String is
      Scale    : Integer := 1;
      Int_Part : Integer;
      Frac_Part : Integer;
      Result   : String (1 .. 16);
      Pos      : Natural := 1;
   begin
      for I in 1 .. Decimals loop
         Scale := Scale * 10;
      end loop;

      -- Value is assumed to be scaled by 256 (8.8 format)
      -- Convert to decimal representation
      Int_Part := abs (Value) / 256;
      Frac_Part := ((abs (Value) mod 256) * Scale) / 256;

      if Value < 0 then
         Result (Pos) := '-';
         Pos := Pos + 1;
      end if;

      declare
         Int_Str : constant String := Int_Image (Int_Part);
      begin
         for C of Int_Str loop
            Result (Pos) := C;
            Pos := Pos + 1;
         end loop;
      end;

      if Decimals > 0 then
         Result (Pos) := '.';
         Pos := Pos + 1;

         declare
            Frac_Str : constant String := Int_Image_Padded (Frac_Part, Decimals, '0');
         begin
            for C of Frac_Str loop
               Result (Pos) := C;
               Pos := Pos + 1;
            end loop;
         end;
      end if;

      return Result (1 .. Pos - 1);
   end Fixed_Image;

   -------------------
   -- Percent_Image --
   -------------------

   function Percent_Image (Value : Natural) return String is
   begin
      return Int_Image (Value) & "%";
   end Percent_Image;

   function Percent_Image (Num, Denom : Natural) return String is
   begin
      if Denom = 0 then
         return "0%";
      end if;
      return Percent_Image ((Num * 100) / Denom);
   end Percent_Image;

   --------------------
   -- Currency_Image --
   --------------------

   function Currency_Image (Cents : Integer) return String is
      Dollars : constant Integer := abs Cents / 100;
      C       : constant Integer := abs Cents mod 100;
   begin
      if Cents < 0 then
         return "-$" & Int_Image (Dollars) & "." & Int_Image_Padded (C, 2);
      else
         return "$" & Int_Image (Dollars) & "." & Int_Image_Padded (C, 2);
      end if;
   end Currency_Image;

   --------------
   -- Time_HMS --
   --------------

   function Time_HMS (Seconds : Natural) return String is
      H : constant Natural := Seconds / 3600;
      M : constant Natural := (Seconds / 60) mod 60;
      S : constant Natural := Seconds mod 60;
   begin
      return Int_Image_Padded (H, 2) & ":" &
             Int_Image_Padded (M, 2) & ":" &
             Int_Image_Padded (S, 2);
   end Time_HMS;

   -------------
   -- Time_MS --
   -------------

   function Time_MS (Seconds : Natural) return String is
      M : constant Natural := Seconds / 60;
      S : constant Natural := Seconds mod 60;
   begin
      return Int_Image_Padded (M, 2) & ":" &
             Int_Image_Padded (S, 2);
   end Time_MS;

   --------------------
   -- Duration_Image --
   --------------------

   function Duration_Image (Seconds : Natural) return String is
      H : constant Natural := Seconds / 3600;
      M : constant Natural := (Seconds / 60) mod 60;
   begin
      if H > 0 then
         if M > 0 then
            return Int_Image (H) & "h " & Int_Image (M) & "m";
         else
            return Int_Image (H) & "h";
         end if;
      elsif M > 0 then
         return Int_Image (M) & "m";
      else
         return Int_Image (Seconds) & "s";
      end if;
   end Duration_Image;

   ----------------
   -- Size_Image --
   ----------------

   function Size_Image (Bytes : Natural) return String is
   begin
      if Bytes >= 1024 * 1024 then
         return Int_Image (Bytes / (1024 * 1024)) & " MB";
      elsif Bytes >= 1024 then
         return Int_Image (Bytes / 1024) & " KB";
      else
         return Int_Image (Bytes) & " B";
      end if;
   end Size_Image;

   -------------
   -- Ordinal --
   -------------

   function Ordinal (N : Positive) return String is
      Last_Digit : constant Natural := N mod 10;
      Last_Two   : constant Natural := N mod 100;
   begin
      if Last_Two >= 11 and Last_Two <= 13 then
         return Int_Image (N) & "th";
      elsif Last_Digit = 1 then
         return Int_Image (N) & "st";
      elsif Last_Digit = 2 then
         return Int_Image (N) & "nd";
      elsif Last_Digit = 3 then
         return Int_Image (N) & "rd";
      else
         return Int_Image (N) & "th";
      end if;
   end Ordinal;

   -----------------
   -- Repeat_Char --
   -----------------

   function Repeat_Char (C : Character; Count : Natural) return String is
   begin
      if Count = 0 then
         return "";
      end if;

      declare
         Result : String (1 .. Count);
      begin
         for I in Result'Range loop
            Result (I) := C;
         end loop;
         return Result;
      end;
   end Repeat_Char;

   ----------
   -- Join --
   ----------

   function Join (S1, S2 : String; Sep : String := ", ") return String is
   begin
      if S1'Length = 0 then
         return S2;
      elsif S2'Length = 0 then
         return S1;
      else
         return S1 & Sep & S2;
      end if;
   end Join;

   function Join (S1, S2, S3 : String; Sep : String := ", ") return String is
   begin
      return Join (Join (S1, S2, Sep), S3, Sep);
   end Join;

   -----------
   -- Quote --
   -----------

   function Quote (S : String) return String is
   begin
      return '"' & S & '"';
   end Quote;

   ------------------
   -- Single_Quote --
   ------------------

   function Single_Quote (S : String) return String is
   begin
      return ''' & S & ''';
   end Single_Quote;

   -------------
   -- Bracket --
   -------------

   function Bracket (S : String) return String is
   begin
      return '[' & S & ']';
   end Bracket;

   -----------
   -- Paren --
   -----------

   function Paren (S : String) return String is
   begin
      return '(' & S & ')';
   end Paren;

   -----------
   -- Brace --
   -----------

   function Brace (S : String) return String is
   begin
      return '{' & S & '}';
   end Brace;

end GNAT.Format;
