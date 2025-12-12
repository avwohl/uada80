-- GNAT.String_Ops body for Z80
-- String manipulation implementation

package body GNAT.String_Ops is

   -----------------
   -- Starts_With --
   -----------------

   function Starts_With (Str : String; Prefix : String) return Boolean is
   begin
      if Prefix'Length > Str'Length then
         return False;
      end if;
      return Str (Str'First .. Str'First + Prefix'Length - 1) = Prefix;
   end Starts_With;

   ---------------
   -- Ends_With --
   ---------------

   function Ends_With (Str : String; Suffix : String) return Boolean is
   begin
      if Suffix'Length > Str'Length then
         return False;
      end if;
      return Str (Str'Last - Suffix'Length + 1 .. Str'Last) = Suffix;
   end Ends_With;

   --------------
   -- Contains --
   --------------

   function Contains (Str : String; Pattern : String) return Boolean is
   begin
      return Index (Str, Pattern) > 0;
   end Contains;

   -----------
   -- Index --
   -----------

   function Index (Str : String; Pattern : String) return Natural is
   begin
      return Index (Str, Pattern, Str'First);
   end Index;

   function Index (Str : String; Pattern : String; From : Positive) return Natural is
   begin
      if Pattern'Length = 0 or Pattern'Length > Str'Length then
         return 0;
      end if;

      for I in From .. Str'Last - Pattern'Length + 1 loop
         if Str (I .. I + Pattern'Length - 1) = Pattern then
            return I;
         end if;
      end loop;
      return 0;
   end Index;

   -------------
   -- Replace --
   -------------

   function Replace
     (Str     : String;
      Pattern : String;
      Replace : String) return String
   is
      Pos : constant Natural := Index (Str, Pattern);
   begin
      if Pos = 0 then
         return Str;
      end if;

      return Str (Str'First .. Pos - 1) & Replace &
             Str (Pos + Pattern'Length .. Str'Last);
   end Replace;

   -----------------
   -- Replace_All --
   -----------------

   function Replace_All
     (Str     : String;
      Pattern : String;
      Replace : String) return String
   is
      Result : String (1 .. Str'Length * 2);  -- Assume max 2x expansion
      R_Len  : Natural := 0;
      I      : Natural := Str'First;
      Match  : Natural;
   begin
      while I <= Str'Last loop
         Match := Index (Str, Pattern, I);
         if Match = 0 then
            -- Copy rest
            Result (R_Len + 1 .. R_Len + Str'Last - I + 1) := Str (I .. Str'Last);
            R_Len := R_Len + Str'Last - I + 1;
            exit;
         else
            -- Copy before match
            Result (R_Len + 1 .. R_Len + Match - I) := Str (I .. Match - 1);
            R_Len := R_Len + Match - I;
            -- Copy replacement
            Result (R_Len + 1 .. R_Len + Replace'Length) := Replace;
            R_Len := R_Len + Replace'Length;
            I := Match + Pattern'Length;
         end if;
      end loop;

      return Result (1 .. R_Len);
   end Replace_All;

   ----------
   -- Trim --
   ----------

   function Trim (Str : String) return String is
   begin
      return Trim_Right (Trim_Left (Str));
   end Trim;

   ---------------
   -- Trim_Left --
   ---------------

   function Trim_Left (Str : String) return String is
   begin
      for I in Str'Range loop
         if Str (I) /= ' ' and Str (I) /= ASCII.HT then
            return Str (I .. Str'Last);
         end if;
      end loop;
      return "";
   end Trim_Left;

   ----------------
   -- Trim_Right --
   ----------------

   function Trim_Right (Str : String) return String is
   begin
      for I in reverse Str'Range loop
         if Str (I) /= ' ' and Str (I) /= ASCII.HT then
            return Str (Str'First .. I);
         end if;
      end loop;
      return "";
   end Trim_Right;

   ------------
   -- Repeat --
   ------------

   function Repeat (Str : String; Count : Natural) return String is
      Result : String (1 .. Str'Length * Count);
   begin
      for I in 0 .. Count - 1 loop
         Result (I * Str'Length + 1 .. (I + 1) * Str'Length) := Str;
      end loop;
      return Result;
   end Repeat;

   ------------
   -- Center --
   ------------

   function Center (Str : String; Width : Natural; Fill : Character := ' ') return String is
      Result   : String (1 .. Width) := (others => Fill);
      Start    : Natural;
   begin
      if Str'Length >= Width then
         return Str (Str'First .. Str'First + Width - 1);
      end if;

      Start := (Width - Str'Length) / 2 + 1;
      Result (Start .. Start + Str'Length - 1) := Str;
      return Result;
   end Center;

end GNAT.String_Ops;
