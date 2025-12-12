-- Ada.Strings.Maps body for Z80
-- Character set and mapping operations implementation

package body Ada.Strings.Maps is

   -------------------
   -- Init_Identity --
   -------------------

   function Init_Identity return Character_Mapping is
      Result : Character_Mapping;
   begin
      for C in Character loop
         Result.Map (C) := C;
      end loop;
      return Result;
   end Init_Identity;

   ------------
   -- To_Set --
   ------------

   function To_Set (Span : Character_Range) return Character_Set is
      Result : Character_Set;
   begin
      for C in Span.Low .. Span.High loop
         Result.Bits (Character'Pos (C)) := True;
      end loop;
      return Result;
   end To_Set;

   function To_Set (Ranges : Character_Ranges) return Character_Set is
      Result : Character_Set;
   begin
      for I in Ranges'Range loop
         for C in Ranges (I).Low .. Ranges (I).High loop
            Result.Bits (Character'Pos (C)) := True;
         end loop;
      end loop;
      return Result;
   end To_Set;

   function To_Set (Sequence : Character_Sequence) return Character_Set is
      Result : Character_Set;
   begin
      for I in Sequence'Range loop
         Result.Bits (Character'Pos (Sequence (I))) := True;
      end loop;
      return Result;
   end To_Set;

   function To_Set (Singleton : Character) return Character_Set is
      Result : Character_Set;
   begin
      Result.Bits (Character'Pos (Singleton)) := True;
      return Result;
   end To_Set;

   ---------------
   -- To_Ranges --
   ---------------

   function To_Ranges (Set : Character_Set) return Character_Ranges is
      -- First count the number of ranges
      Count    : Natural := 0;
      In_Range : Boolean := False;
   begin
      for I in 0 .. 255 loop
         if Set.Bits (I) then
            if not In_Range then
               Count := Count + 1;
               In_Range := True;
            end if;
         else
            In_Range := False;
         end if;
      end loop;

      declare
         Result : Character_Ranges (1 .. Count);
         Idx    : Natural := 0;
         Start  : Natural;
      begin
         In_Range := False;
         for I in 0 .. 255 loop
            if Set.Bits (I) then
               if not In_Range then
                  In_Range := True;
                  Start := I;
               end if;
            else
               if In_Range then
                  Idx := Idx + 1;
                  Result (Idx).Low := Character'Val (Start);
                  Result (Idx).High := Character'Val (I - 1);
                  In_Range := False;
               end if;
            end if;
         end loop;

         -- Handle case where set extends to character 255
         if In_Range then
            Idx := Idx + 1;
            Result (Idx).Low := Character'Val (Start);
            Result (Idx).High := Character'Val (255);
         end if;

         return Result;
      end;
   end To_Ranges;

   -----------------
   -- To_Sequence --
   -----------------

   function To_Sequence (Set : Character_Set) return Character_Sequence is
      Count : Natural := 0;
   begin
      -- Count characters in set
      for I in 0 .. 255 loop
         if Set.Bits (I) then
            Count := Count + 1;
         end if;
      end loop;

      declare
         Result : String (1 .. Count);
         Idx    : Natural := 0;
      begin
         for I in 0 .. 255 loop
            if Set.Bits (I) then
               Idx := Idx + 1;
               Result (Idx) := Character'Val (I);
            end if;
         end loop;
         return Result;
      end;
   end To_Sequence;

   -----------
   -- Is_In --
   -----------

   function Is_In (Element : Character; Set : Character_Set) return Boolean is
   begin
      return Set.Bits (Character'Pos (Element));
   end Is_In;

   ---------------
   -- Is_Subset --
   ---------------

   function Is_Subset (Elements : Character_Set; Of_Set : Character_Set) return Boolean is
   begin
      for I in 0 .. 255 loop
         if Elements.Bits (I) and not Of_Set.Bits (I) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Subset;

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Character_Set) return Character_Set is
      Result : Character_Set;
   begin
      for I in 0 .. 255 loop
         Result.Bits (I) := Left.Bits (I) or Right.Bits (I);
      end loop;
      return Result;
   end "or";

   -----------
   -- "and" --
   -----------

   function "and" (Left, Right : Character_Set) return Character_Set is
      Result : Character_Set;
   begin
      for I in 0 .. 255 loop
         Result.Bits (I) := Left.Bits (I) and Right.Bits (I);
      end loop;
      return Result;
   end "and";

   -----------
   -- "xor" --
   -----------

   function "xor" (Left, Right : Character_Set) return Character_Set is
      Result : Character_Set;
   begin
      for I in 0 .. 255 loop
         Result.Bits (I) := Left.Bits (I) xor Right.Bits (I);
      end loop;
      return Result;
   end "xor";

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Character_Set) return Character_Set is
      Result : Character_Set;
   begin
      for I in 0 .. 255 loop
         Result.Bits (I) := Left.Bits (I) and not Right.Bits (I);
      end loop;
      return Result;
   end "-";

   -----------
   -- "not" --
   -----------

   function "not" (Right : Character_Set) return Character_Set is
      Result : Character_Set;
   begin
      for I in 0 .. 255 loop
         Result.Bits (I) := not Right.Bits (I);
      end loop;
      return Result;
   end "not";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Character_Set) return Boolean is
   begin
      for I in 0 .. 255 loop
         if Left.Bits (I) /= Right.Bits (I) then
            return False;
         end if;
      end loop;
      return True;
   end "=";

   ----------------
   -- To_Mapping --
   ----------------

   function To_Mapping (From, To : Character_Sequence) return Character_Mapping is
      Result : Character_Mapping := Identity;
   begin
      if From'Length /= To'Length then
         raise Translation_Error;
      end if;

      -- Check for duplicate characters in From
      for I in From'Range loop
         for J in From'First .. I - 1 loop
            if From (I) = From (J) then
               raise Translation_Error;
            end if;
         end loop;
      end loop;

      -- Build the mapping
      for I in From'Range loop
         Result.Map (From (I)) := To (To'First + I - From'First);
      end loop;

      return Result;
   end To_Mapping;

   ---------------
   -- To_Domain --
   ---------------

   function To_Domain (Map : Character_Mapping) return Character_Sequence is
      Count : Natural := 0;
   begin
      -- Count non-identity mappings
      for C in Character loop
         if Map.Map (C) /= C then
            Count := Count + 1;
         end if;
      end loop;

      declare
         Result : String (1 .. Count);
         Idx    : Natural := 0;
      begin
         for C in Character loop
            if Map.Map (C) /= C then
               Idx := Idx + 1;
               Result (Idx) := C;
            end if;
         end loop;
         return Result;
      end;
   end To_Domain;

   --------------
   -- To_Range --
   --------------

   function To_Range (Map : Character_Mapping) return Character_Sequence is
      Count : Natural := 0;
   begin
      -- Count non-identity mappings
      for C in Character loop
         if Map.Map (C) /= C then
            Count := Count + 1;
         end if;
      end loop;

      declare
         Result : String (1 .. Count);
         Idx    : Natural := 0;
      begin
         for C in Character loop
            if Map.Map (C) /= C then
               Idx := Idx + 1;
               Result (Idx) := Map.Map (C);
            end if;
         end loop;
         return Result;
      end;
   end To_Range;

   -----------
   -- Value --
   -----------

   function Value (Map : Character_Mapping; Element : Character) return Character is
   begin
      return Map.Map (Element);
   end Value;

end Ada.Strings.Maps;
