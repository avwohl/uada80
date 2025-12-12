-- GNAT.String_Utils body for Z80
-- Additional string utilities implementation

package body GNAT.String_Utils is

   -----------
   -- Count --
   -----------

   function Count
     (Source  : String;
      Pattern : Character) return Natural
   is
      Result : Natural := 0;
   begin
      for I in Source'Range loop
         if Source (I) = Pattern then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count;

   -----------
   -- Count --
   -----------

   function Count
     (Source  : String;
      Pattern : String) return Natural
   is
      Result : Natural := 0;
      I      : Natural := Source'First;
   begin
      if Pattern'Length = 0 or Pattern'Length > Source'Length then
         return 0;
      end if;

      while I <= Source'Last - Pattern'Length + 1 loop
         if Source (I .. I + Pattern'Length - 1) = Pattern then
            Result := Result + 1;
            I := I + Pattern'Length;
         else
            I := I + 1;
         end if;
      end loop;
      return Result;
   end Count;

   -----------------------
   -- Replace_Character --
   -----------------------

   function Replace_Character
     (S    : String;
      From : Character;
      To   : Character) return String
   is
      Result : String := S;
   begin
      for I in Result'Range loop
         if Result (I) = From then
            Result (I) := To;
         end if;
      end loop;
      return Result;
   end Replace_Character;

   ----------------------
   -- Remove_Character --
   ----------------------

   function Remove_Character
     (S : String;
      C : Character) return String
   is
      Result : String (1 .. S'Length);
      Idx    : Natural := 0;
   begin
      for I in S'Range loop
         if S (I) /= C then
            Idx := Idx + 1;
            Result (Idx) := S (I);
         end if;
      end loop;
      return Result (1 .. Idx);
   end Remove_Character;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate
     (S     : String;
      Count : Natural) return String
   is
   begin
      if Count = 0 then
         return "";
      end if;

      declare
         Result : String (1 .. S'Length * Count);
         Pos    : Natural := 1;
      begin
         for I in 1 .. Count loop
            Result (Pos .. Pos + S'Length - 1) := S;
            Pos := Pos + S'Length;
         end loop;
         return Result;
      end;
   end Duplicate;

   --------------------
   -- Reverse_String --
   --------------------

   function Reverse_String (S : String) return String is
      Result : String (S'Range);
   begin
      for I in S'Range loop
         Result (S'Last - I + S'First) := S (I);
      end loop;
      return Result;
   end Reverse_String;

   ---------------
   -- Is_Prefix --
   ---------------

   function Is_Prefix
     (Prefix : String;
      S      : String) return Boolean
   is
   begin
      if Prefix'Length > S'Length then
         return False;
      end if;
      return S (S'First .. S'First + Prefix'Length - 1) = Prefix;
   end Is_Prefix;

   ---------------
   -- Is_Suffix --
   ---------------

   function Is_Suffix
     (Suffix : String;
      S      : String) return Boolean
   is
   begin
      if Suffix'Length > S'Length then
         return False;
      end if;
      return S (S'Last - Suffix'Length + 1 .. S'Last) = Suffix;
   end Is_Suffix;

end GNAT.String_Utils;
