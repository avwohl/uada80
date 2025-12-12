-- Ada.Strings.Wide_Maps body for Z80
-- Wide character mapping and sets implementation

package body Ada.Strings.Wide_Maps is

   ------------
   -- To_Set --
   ------------

   function To_Set (Ranges : Wide_Character_Ranges) return Wide_Character_Set is
      Result : Wide_Character_Set;
   begin
      for R in Ranges'Range loop
         for C in Ranges (R).Low .. Ranges (R).High loop
            if Result.Length < Max_Set_Size then
               -- Check if already in set
               declare
                  Found : Boolean := False;
               begin
                  for I in 1 .. Result.Length loop
                     if Result.Chars (I) = C then
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Result.Length := Result.Length + 1;
                     Result.Chars (Result.Length) := C;
                  end if;
               end;
            end if;
         end loop;
      end loop;
      return Result;
   end To_Set;

   function To_Set (Span : Wide_Character_Range) return Wide_Character_Set is
   begin
      return To_Set (Wide_Character_Ranges'(1 => Span));
   end To_Set;

   function To_Set (Sequence : Wide_Character_Sequence) return Wide_Character_Set is
      Result : Wide_Character_Set;
   begin
      for I in Sequence'Range loop
         if Result.Length < Max_Set_Size then
            declare
               Found : Boolean := False;
            begin
               for J in 1 .. Result.Length loop
                  if Result.Chars (J) = Sequence (I) then
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  Result.Length := Result.Length + 1;
                  Result.Chars (Result.Length) := Sequence (I);
               end if;
            end;
         end if;
      end loop;
      return Result;
   end To_Set;

   function To_Set (Singleton : Wide_Character) return Wide_Character_Set is
      Result : Wide_Character_Set;
   begin
      Result.Chars (1) := Singleton;
      Result.Length := 1;
      return Result;
   end To_Set;

   ---------------
   -- To_Ranges --
   ---------------

   function To_Ranges (Set : Wide_Character_Set) return Wide_Character_Ranges is
      -- Simplified: return each character as its own range
      Result : Wide_Character_Ranges (1 .. Set.Length);
   begin
      for I in 1 .. Set.Length loop
         Result (I) := (Low => Set.Chars (I), High => Set.Chars (I));
      end loop;
      return Result;
   end To_Ranges;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence (Set : Wide_Character_Set) return Wide_Character_Sequence is
   begin
      return Set.Chars (1 .. Set.Length);
   end To_Sequence;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Wide_Character_Set) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      -- Check if all chars in Left are in Right
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

   function "not" (Right : Wide_Character_Set) return Wide_Character_Set is
      Result : Wide_Character_Set;
   begin
      -- Add all Latin-1 characters not in Right (limited for Z80)
      for C in Wide_Character'Val (32) .. Wide_Character'Val (126) loop
         if not Is_In (C, Right) and Result.Length < Max_Set_Size then
            Result.Length := Result.Length + 1;
            Result.Chars (Result.Length) := C;
         end if;
      end loop;
      return Result;
   end "not";

   -----------
   -- "and" --
   -----------

   function "and" (Left, Right : Wide_Character_Set) return Wide_Character_Set is
      Result : Wide_Character_Set;
   begin
      for I in 1 .. Left.Length loop
         if Is_In (Left.Chars (I), Right) and Result.Length < Max_Set_Size then
            Result.Length := Result.Length + 1;
            Result.Chars (Result.Length) := Left.Chars (I);
         end if;
      end loop;
      return Result;
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Wide_Character_Set) return Wide_Character_Set is
      Result : Wide_Character_Set := Left;
   begin
      for I in 1 .. Right.Length loop
         if not Is_In (Right.Chars (I), Result) and Result.Length < Max_Set_Size then
            Result.Length := Result.Length + 1;
            Result.Chars (Result.Length) := Right.Chars (I);
         end if;
      end loop;
      return Result;
   end "or";

   -----------
   -- "xor" --
   -----------

   function "xor" (Left, Right : Wide_Character_Set) return Wide_Character_Set is
      Result : Wide_Character_Set;
   begin
      -- Add chars in Left but not in Right
      for I in 1 .. Left.Length loop
         if not Is_In (Left.Chars (I), Right) and Result.Length < Max_Set_Size then
            Result.Length := Result.Length + 1;
            Result.Chars (Result.Length) := Left.Chars (I);
         end if;
      end loop;

      -- Add chars in Right but not in Left
      for I in 1 .. Right.Length loop
         if not Is_In (Right.Chars (I), Left) and Result.Length < Max_Set_Size then
            Result.Length := Result.Length + 1;
            Result.Chars (Result.Length) := Right.Chars (I);
         end if;
      end loop;

      return Result;
   end "xor";

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Wide_Character_Set) return Wide_Character_Set is
      Result : Wide_Character_Set;
   begin
      for I in 1 .. Left.Length loop
         if not Is_In (Left.Chars (I), Right) and Result.Length < Max_Set_Size then
            Result.Length := Result.Length + 1;
            Result.Chars (Result.Length) := Left.Chars (I);
         end if;
      end loop;
      return Result;
   end "-";

   -----------
   -- Is_In --
   -----------

   function Is_In
     (Element : Wide_Character;
      Set     : Wide_Character_Set) return Boolean
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
     (Elements : Wide_Character_Set;
      Set      : Wide_Character_Set) return Boolean
   is
   begin
      for I in 1 .. Elements.Length loop
         if not Is_In (Elements.Chars (I), Set) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Subset;

   -----------
   -- Value --
   -----------

   function Value
     (Map     : Wide_Character_Mapping;
      Element : Wide_Character) return Wide_Character
   is
   begin
      for I in 1 .. Map.Length loop
         if Map.Domain (I) = Element then
            return Map.Ranges (I);
         end if;
      end loop;
      return Element;  -- Identity mapping for unmapped chars
   end Value;

   ----------------
   -- To_Mapping --
   ----------------

   function To_Mapping
     (From, To : Wide_Character_Sequence) return Wide_Character_Mapping
   is
      Result : Wide_Character_Mapping;
      Len    : constant Natural := Natural'Min (From'Length, To'Length);
   begin
      if Len > Max_Map_Size then
         raise Constraint_Error;
      end if;

      Result.Length := Len;
      for I in 0 .. Len - 1 loop
         Result.Domain (I + 1) := From (From'First + I);
         Result.Ranges (I + 1) := To (To'First + I);
      end loop;

      return Result;
   end To_Mapping;

   ---------------
   -- To_Domain --
   ---------------

   function To_Domain (Map : Wide_Character_Mapping) return Wide_Character_Sequence is
   begin
      return Map.Domain (1 .. Map.Length);
   end To_Domain;

   --------------
   -- To_Range --
   --------------

   function To_Range (Map : Wide_Character_Mapping) return Wide_Character_Sequence is
   begin
      return Map.Ranges (1 .. Map.Length);
   end To_Range;

end Ada.Strings.Wide_Maps;
