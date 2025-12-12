-- GNAT.Spitbol body for Z80
-- SPITBOL-style pattern matching utilities implementation

package body GNAT.Spitbol is

   ---------
   -- Dup --
   ---------

   function Dup (Str : String; Count : Natural) return String is
      Result : String (1 .. Str'Length * Count);
      Pos    : Natural := 1;
   begin
      for I in 1 .. Count loop
         Result (Pos .. Pos + Str'Length - 1) := Str;
         Pos := Pos + Str'Length;
      end loop;
      return Result;
   end Dup;

   ----------
   -- Lpad --
   ----------

   function Lpad (Str : String; Len : Natural; Pad : Character := ' ') return String is
   begin
      if Str'Length >= Len then
         return Str;
      end if;

      declare
         Result : String (1 .. Len) := (others => Pad);
      begin
         Result (Len - Str'Length + 1 .. Len) := Str;
         return Result;
      end;
   end Lpad;

   ----------
   -- Rpad --
   ----------

   function Rpad (Str : String; Len : Natural; Pad : Character := ' ') return String is
   begin
      if Str'Length >= Len then
         return Str;
      end if;

      declare
         Result : String (1 .. Len) := (others => Pad);
      begin
         Result (1 .. Str'Length) := Str;
         return Result;
      end;
   end Rpad;

   ----------
   -- Trim --
   ----------

   function Trim (Str : String) return String is
      First : Natural := Str'First;
      Last  : Natural := Str'Last;
   begin
      while First <= Last and then Str (First) = ' ' loop
         First := First + 1;
      end loop;

      while Last >= First and then Str (Last) = ' ' loop
         Last := Last - 1;
      end loop;

      if First > Last then
         return "";
      end if;

      return Str (First .. Last);
   end Trim;

   -----------
   -- Ltrim --
   -----------

   function Ltrim (Str : String) return String is
      First : Natural := Str'First;
   begin
      while First <= Str'Last and then Str (First) = ' ' loop
         First := First + 1;
      end loop;

      if First > Str'Last then
         return "";
      end if;

      return Str (First .. Str'Last);
   end Ltrim;

   -----------
   -- Rtrim --
   -----------

   function Rtrim (Str : String) return String is
      Last : Natural := Str'Last;
   begin
      while Last >= Str'First and then Str (Last) = ' ' loop
         Last := Last - 1;
      end loop;

      if Last < Str'First then
         return "";
      end if;

      return Str (Str'First .. Last);
   end Rtrim;

   ---------
   -- Sub --
   ---------

   function Sub
     (Source  : String;
      Start   : Positive;
      Len     : Natural;
      Replace : String) return String
   is
      Before : constant String := Source (Source'First .. Start - 1);
      After  : constant String := (if Start + Len - 1 < Source'Last
                                   then Source (Start + Len .. Source'Last)
                                   else "");
   begin
      return Before & Replace & After;
   end Sub;

   --------------------
   -- Reverse_String --
   --------------------

   function Reverse_String (Str : String) return String is
      Result : String (Str'Range);
   begin
      for I in Str'Range loop
         Result (Str'Last - I + Str'First) := Str (I);
      end loop;
      return Result;
   end Reverse_String;

end GNAT.Spitbol;
