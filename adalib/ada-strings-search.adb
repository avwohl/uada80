-- Ada.Strings.Search body for Z80
-- String search implementation

package body Ada.Strings.Search is

   ----------------
   -- Is_In_Set --
   ----------------

   function Is_In_Set (Char : Character; Set : String) return Boolean is
   begin
      for I in Set'Range loop
         if Set (I) = Char then
            return True;
         end if;
      end loop;
      return False;
   end Is_In_Set;

   -----------
   -- Count --
   -----------

   function Count
     (Source  : String;
      Pattern : String) return Natural
   is
      Result : Natural := 0;
      Pos    : Natural;
   begin
      if Pattern'Length = 0 or Source'Length < Pattern'Length then
         return 0;
      end if;

      Pos := Source'First;
      while Pos <= Source'Last - Pattern'Length + 1 loop
         if Source (Pos .. Pos + Pattern'Length - 1) = Pattern then
            Result := Result + 1;
            Pos := Pos + Pattern'Length;  -- Non-overlapping
         else
            Pos := Pos + 1;
         end if;
      end loop;

      return Result;
   end Count;

   function Count
     (Source : String;
      Set    : String) return Natural
   is
      Result : Natural := 0;
   begin
      for I in Source'Range loop
         if Is_In_Set (Source (I), Set) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward) return Natural
   is
   begin
      if Pattern'Length = 0 or Source'Length < Pattern'Length then
         return 0;
      end if;

      if Going = Forward then
         for I in Source'First .. Source'Last - Pattern'Length + 1 loop
            if Source (I .. I + Pattern'Length - 1) = Pattern then
               return I;
            end if;
         end loop;
      else
         for I in reverse Source'First .. Source'Last - Pattern'Length + 1 loop
            if Source (I .. I + Pattern'Length - 1) = Pattern then
               return I;
            end if;
         end loop;
      end if;

      return 0;
   end Index;

   function Index
     (Source : String;
      Set    : String;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural
   is
      In_Set : Boolean;
   begin
      if Set'Length = 0 then
         return 0;
      end if;

      if Going = Forward then
         for I in Source'Range loop
            In_Set := Is_In_Set (Source (I), Set);
            if (Test = Inside and In_Set) or (Test = Outside and not In_Set) then
               return I;
            end if;
         end loop;
      else
         for I in reverse Source'Range loop
            In_Set := Is_In_Set (Source (I), Set);
            if (Test = Inside and In_Set) or (Test = Outside and not In_Set) then
               return I;
            end if;
         end loop;
      end if;

      return 0;
   end Index;

   ---------------------
   -- Index_Non_Blank --
   ---------------------

   function Index_Non_Blank
     (Source : String;
      Going  : Direction := Forward) return Natural
   is
   begin
      if Going = Forward then
         for I in Source'Range loop
            if Source (I) /= ' ' then
               return I;
            end if;
         end loop;
      else
         for I in reverse Source'Range loop
            if Source (I) /= ' ' then
               return I;
            end if;
         end loop;
      end if;

      return 0;
   end Index_Non_Blank;

end Ada.Strings.Search;
