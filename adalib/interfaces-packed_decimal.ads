-- Interfaces.Packed_Decimal for Z80
-- Packed decimal (BCD) arithmetic

package Interfaces.Packed_Decimal is
   pragma Pure;

   -- Packed decimal types (BCD encoding)
   type Packed_Decimal is private;

   Max_Digits : constant := 18;  -- Maximum digits supported

   -- Conversion
   function To_Packed (Value : Long_Long_Integer; Digits : Positive) return Packed_Decimal;
   function To_Integer (Value : Packed_Decimal) return Long_Long_Integer;
   function To_String (Value : Packed_Decimal) return String;

   -- Arithmetic
   function "+" (Left, Right : Packed_Decimal) return Packed_Decimal;
   function "-" (Left, Right : Packed_Decimal) return Packed_Decimal;
   function "*" (Left, Right : Packed_Decimal) return Packed_Decimal;
   function "/" (Left, Right : Packed_Decimal) return Packed_Decimal;

   function "abs" (Value : Packed_Decimal) return Packed_Decimal;
   function "-" (Value : Packed_Decimal) return Packed_Decimal;

   -- Comparison
   function "=" (Left, Right : Packed_Decimal) return Boolean;
   function "<" (Left, Right : Packed_Decimal) return Boolean;
   function "<=" (Left, Right : Packed_Decimal) return Boolean;
   function ">" (Left, Right : Packed_Decimal) return Boolean;
   function ">=" (Left, Right : Packed_Decimal) return Boolean;

   -- Properties
   function Is_Zero (Value : Packed_Decimal) return Boolean;
   function Is_Negative (Value : Packed_Decimal) return Boolean;
   function Digit_Count (Value : Packed_Decimal) return Natural;

private
   -- Store as array of BCD nibbles plus sign
   type BCD_Array is array (1 .. Max_Digits) of Natural range 0 .. 9;

   type Packed_Decimal is record
      Digits   : BCD_Array := (others => 0);
      Length   : Natural := 0;
      Negative : Boolean := False;
   end record;

end Interfaces.Packed_Decimal;
