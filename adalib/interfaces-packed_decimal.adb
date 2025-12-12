-- Interfaces.Packed_Decimal body for Z80
-- Packed decimal implementation

package body Interfaces.Packed_Decimal is

   ---------------
   -- To_Packed --
   ---------------

   function To_Packed (Value : Long_Long_Integer; Digits : Positive) return Packed_Decimal is
      Result : Packed_Decimal;
      V      : Long_Long_Integer := abs Value;
      I      : Natural := 0;
   begin
      Result.Negative := Value < 0;
      Result.Length := Digits;

      while V > 0 and I < Max_Digits loop
         I := I + 1;
         Result.Digits (I) := Natural (V mod 10);
         V := V / 10;
      end loop;

      return Result;
   end To_Packed;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Value : Packed_Decimal) return Long_Long_Integer is
      Result : Long_Long_Integer := 0;
      Factor : Long_Long_Integer := 1;
   begin
      for I in 1 .. Value.Length loop
         Result := Result + Long_Long_Integer (Value.Digits (I)) * Factor;
         Factor := Factor * 10;
      end loop;

      if Value.Negative then
         Result := -Result;
      end if;

      return Result;
   end To_Integer;

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Packed_Decimal) return String is
      Result : String (1 .. Value.Length + 1);
      Pos    : Natural := 1;
   begin
      if Value.Negative then
         Result (1) := '-';
         Pos := 2;
      end if;

      for I in reverse 1 .. Value.Length loop
         Result (Pos) := Character'Val (Character'Pos ('0') + Value.Digits (I));
         Pos := Pos + 1;
      end loop;

      if Value.Negative then
         return Result (1 .. Value.Length + 1);
      else
         return Result (1 .. Value.Length);
      end if;
   end To_String;

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Packed_Decimal) return Packed_Decimal is
   begin
      return To_Packed (To_Integer (Left) + To_Integer (Right),
                        Natural'Max (Left.Length, Right.Length));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Packed_Decimal) return Packed_Decimal is
   begin
      return To_Packed (To_Integer (Left) - To_Integer (Right),
                        Natural'Max (Left.Length, Right.Length));
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Packed_Decimal) return Packed_Decimal is
   begin
      return To_Packed (To_Integer (Left) * To_Integer (Right),
                        Left.Length + Right.Length);
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Packed_Decimal) return Packed_Decimal is
   begin
      return To_Packed (To_Integer (Left) / To_Integer (Right), Left.Length);
   end "/";

   -----------
   -- "abs" --
   -----------

   function "abs" (Value : Packed_Decimal) return Packed_Decimal is
      Result : Packed_Decimal := Value;
   begin
      Result.Negative := False;
      return Result;
   end "abs";

   ---------
   -- "-" --
   ---------

   function "-" (Value : Packed_Decimal) return Packed_Decimal is
      Result : Packed_Decimal := Value;
   begin
      Result.Negative := not Value.Negative;
      return Result;
   end "-";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Packed_Decimal) return Boolean is
   begin
      return To_Integer (Left) = To_Integer (Right);
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Packed_Decimal) return Boolean is
   begin
      return To_Integer (Left) < To_Integer (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Packed_Decimal) return Boolean is
   begin
      return To_Integer (Left) <= To_Integer (Right);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Packed_Decimal) return Boolean is
   begin
      return To_Integer (Left) > To_Integer (Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Packed_Decimal) return Boolean is
   begin
      return To_Integer (Left) >= To_Integer (Right);
   end ">=";

   -------------
   -- Is_Zero --
   -------------

   function Is_Zero (Value : Packed_Decimal) return Boolean is
   begin
      for I in 1 .. Value.Length loop
         if Value.Digits (I) /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Zero;

   -----------------
   -- Is_Negative --
   -----------------

   function Is_Negative (Value : Packed_Decimal) return Boolean is
   begin
      return Value.Negative and not Is_Zero (Value);
   end Is_Negative;

   -----------------
   -- Digit_Count --
   -----------------

   function Digit_Count (Value : Packed_Decimal) return Natural is
   begin
      return Value.Length;
   end Digit_Count;

end Interfaces.Packed_Decimal;
