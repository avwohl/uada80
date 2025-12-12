-- Ada.Strings.Wide_Wide_Maps body for Z80
-- Wide_Wide_Character mapping support implementation

package body Ada.Strings.Wide_Wide_Maps is

   ------------
   -- To_Set --
   ------------

   function To_Set (Ranges : Wide_Wide_Character_Ranges) return Wide_Wide_Character_Set is
      Result : Wide_Wide_Character_Set;
   begin
      for R of Ranges loop
         for C in Wide_Wide_Character range R.Low .. R.High loop
            if Result.Length < Max_Set_Size then
               Result.Length := Result.Length + 1;
               Result.Chars (Result.Length) := C;
            end if;
         end loop;
      end loop;
      return Result;
   end To_Set;

   function To_Set (Span : Wide_Wide_Character_Range) return Wide_Wide_Character_Set is
   begin
      return To_Set ((1 => Span));
   end To_Set;

   ---------------
   -- To_Ranges --
   ---------------

   function To_Ranges (Set : Wide_Wide_Character_Set) return Wide_Wide_Character_Ranges is
      Result : Wide_Wide_Character_Ranges (1 .. Set.Length);
   begin
      for I in 1 .. Set.Length loop
         Result (I) := (Low => Set.Chars (I), High => Set.Chars (I));
      end loop;
      return Result;
   end To_Ranges;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Wide_Wide_Character_Set) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;
      for I in 1 .. Left.Length loop
         declare
            Found : Boolean := False;
         begin
            for J in 1 .. Right.Length loop
               if Left.Chars (I) = Right.Chars (J) then
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found then
               return False;
            end if;
         end;
      end loop;
      return True;
   end "=";

   -----------
   -- "not" --
   -----------

   function "not" (Right : Wide_Wide_Character_Set) return Wide_Wide_Character_Set is
      Result : Wide_Wide_Character_Set;
   begin
      -- For Z80, limited to ASCII range complement
      for I in 0 .. 127 loop
         declare
            C : constant Wide_Wide_Character := Wide_Wide_Character'Val (I);
            In_Set : Boolean := False;
         begin
            for J in 1 .. Right.Length loop
               if Right.Chars (J) = C then
                  In_Set := True;
                  exit;
               end if;
            end loop;
            if not In_Set and Result.Length < Max_Set_Size then
               Result.Length := Result.Length + 1;
               Result.Chars (Result.Length) := C;
            end if;
         end;
      end loop;
      return Result;
   end "not";

   -----------
   -- "and" --
   -----------

   function "and" (Left, Right : Wide_Wide_Character_Set) return Wide_Wide_Character_Set is
      Result : Wide_Wide_Character_Set;
   begin
      for I in 1 .. Left.Length loop
         for J in 1 .. Right.Length loop
            if Left.Chars (I) = Right.Chars (J) then
               if Result.Length < Max_Set_Size then
                  Result.Length := Result.Length + 1;
                  Result.Chars (Result.Length) := Left.Chars (I);
               end if;
               exit;
            end if;
         end loop;
      end loop;
      return Result;
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Wide_Wide_Character_Set) return Wide_Wide_Character_Set is
      Result : Wide_Wide_Character_Set := Left;
   begin
      for I in 1 .. Right.Length loop
         declare
            Found : Boolean := False;
         begin
            for J in 1 .. Result.Length loop
               if Result.Chars (J) = Right.Chars (I) then
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found and Result.Length < Max_Set_Size then
               Result.Length := Result.Length + 1;
               Result.Chars (Result.Length) := Right.Chars (I);
            end if;
         end;
      end loop;
      return Result;
   end "or";

   -----------
   -- "xor" --
   -----------

   function "xor" (Left, Right : Wide_Wide_Character_Set) return Wide_Wide_Character_Set is
      Result : Wide_Wide_Character_Set;
   begin
      -- Elements in Left but not in Right
      for I in 1 .. Left.Length loop
         declare
            Found : Boolean := False;
         begin
            for J in 1 .. Right.Length loop
               if Left.Chars (I) = Right.Chars (J) then
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found and Result.Length < Max_Set_Size then
               Result.Length := Result.Length + 1;
               Result.Chars (Result.Length) := Left.Chars (I);
            end if;
         end;
      end loop;

      -- Elements in Right but not in Left
      for I in 1 .. Right.Length loop
         declare
            Found : Boolean := False;
         begin
            for J in 1 .. Left.Length loop
               if Right.Chars (I) = Left.Chars (J) then
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found and Result.Length < Max_Set_Size then
               Result.Length := Result.Length + 1;
               Result.Chars (Result.Length) := Right.Chars (I);
            end if;
         end;
      end loop;

      return Result;
   end "xor";

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Wide_Wide_Character_Set) return Wide_Wide_Character_Set is
      Result : Wide_Wide_Character_Set;
   begin
      for I in 1 .. Left.Length loop
         declare
            Found : Boolean := False;
         begin
            for J in 1 .. Right.Length loop
               if Left.Chars (I) = Right.Chars (J) then
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found and Result.Length < Max_Set_Size then
               Result.Length := Result.Length + 1;
               Result.Chars (Result.Length) := Left.Chars (I);
            end if;
         end;
      end loop;
      return Result;
   end "-";

   -----------
   -- Is_In --
   -----------

   function Is_In
     (Element : Wide_Wide_Character;
      Set     : Wide_Wide_Character_Set) return Boolean
   is
   begin
      for I in 1 .. Set.Length loop
         if Set.Chars (I) = Element then
            return True;
         end if;
      end loop;
      return False;
   end Is_In;

   ---------------
   -- Is_Subset --
   ---------------

   function Is_Subset
     (Elements : Wide_Wide_Character_Set;
      Set      : Wide_Wide_Character_Set) return Boolean
   is
   begin
      for I in 1 .. Elements.Length loop
         if not Is_In (Elements.Chars (I), Set) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Subset;

   ----------------
   -- To_Mapping --
   ----------------

   function To_Mapping
     (From, To : Wide_Wide_String) return Wide_Wide_Character_Mapping
   is
      Result : Wide_Wide_Character_Mapping;
   begin
      if From'Length /= To'Length then
         raise Constraint_Error;
      end if;

      Result.Length := Natural'Min (From'Length, Max_Set_Size);
      for I in 0 .. Result.Length - 1 loop
         Result.Entries (I + 1).From := From (From'First + I);
         Result.Entries (I + 1).To := To (To'First + I);
      end loop;

      return Result;
   end To_Mapping;

   ---------------
   -- To_Domain --
   ---------------

   function To_Domain
     (Map : Wide_Wide_Character_Mapping) return Wide_Wide_String
   is
      Result : Wide_Wide_String (1 .. Map.Length);
   begin
      for I in 1 .. Map.Length loop
         Result (I) := Map.Entries (I).From;
      end loop;
      return Result;
   end To_Domain;

   --------------
   -- To_Range --
   --------------

   function To_Range
     (Map : Wide_Wide_Character_Mapping) return Wide_Wide_String
   is
      Result : Wide_Wide_String (1 .. Map.Length);
   begin
      for I in 1 .. Map.Length loop
         Result (I) := Map.Entries (I).To;
      end loop;
      return Result;
   end To_Range;

   -----------
   -- Value --
   -----------

   function Value
     (Map     : Wide_Wide_Character_Mapping;
      Element : Wide_Wide_Character) return Wide_Wide_Character
   is
   begin
      for I in 1 .. Map.Length loop
         if Map.Entries (I).From = Element then
            return Map.Entries (I).To;
         end if;
      end loop;
      return Element;
   end Value;

end Ada.Strings.Wide_Wide_Maps;
