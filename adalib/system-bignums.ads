-- System.Bignums for Z80
-- Arbitrary precision integer arithmetic

package System.Bignums is
   pragma Preelaborate;

   -- Maximum size of bignum (in 16-bit words)
   Max_Bignum_Words : constant := 16;  -- 256 bits max

   -- Bignum representation
   type Bignum_Word is mod 2 ** 16;
   type Bignum_Data is array (Natural range <>) of Bignum_Word;

   type Bignum (Length : Natural) is record
      Sign : Boolean := False;  -- True if negative
      Data : Bignum_Data (0 .. Length - 1);
   end record;

   -- Basic operations
   function To_Bignum (Value : Integer) return Bignum;
   function To_Bignum (Value : Long_Long_Integer) return Bignum;
   function To_Integer (Value : Bignum) return Integer;
   function To_Long_Long_Integer (Value : Bignum) return Long_Long_Integer;

   -- Arithmetic
   function "+" (Left, Right : Bignum) return Bignum;
   function "-" (Left, Right : Bignum) return Bignum;
   function "-" (Right : Bignum) return Bignum;
   function "*" (Left, Right : Bignum) return Bignum;
   function "/" (Left, Right : Bignum) return Bignum;
   function "mod" (Left, Right : Bignum) return Bignum;
   function "rem" (Left, Right : Bignum) return Bignum;

   -- Comparisons
   function "=" (Left, Right : Bignum) return Boolean;
   function "<" (Left, Right : Bignum) return Boolean;
   function "<=" (Left, Right : Bignum) return Boolean;
   function ">" (Left, Right : Bignum) return Boolean;
   function ">=" (Left, Right : Bignum) return Boolean;

   -- Utilities
   function Is_Zero (Value : Bignum) return Boolean;
   function Abs_Value (Value : Bignum) return Bignum;

end System.Bignums;
