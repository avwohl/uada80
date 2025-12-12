-- System.Arith_64 for Z80
-- 64-bit arithmetic operations
--
-- Provides 64-bit arithmetic support for Z80 (software emulated)

with Interfaces;

package System.Arith_64 is
   pragma Pure;

   subtype Int64 is Interfaces.Integer_64;
   subtype Uns64 is Interfaces.Unsigned_64;
   subtype Uns32 is Interfaces.Unsigned_32;

   -- Addition with overflow check
   function Add_With_Ovflo_Check (X, Y : Int64) return Int64;

   -- Subtraction with overflow check
   function Subtract_With_Ovflo_Check (X, Y : Int64) return Int64;

   -- Multiplication with overflow check
   function Multiply_With_Ovflo_Check (X, Y : Int64) return Int64;

   -- Double-word division
   procedure Double_Divide
     (X, Y, Z : Int64;
      Q, R    : out Int64);
   -- Computes Q = (X * Y) / Z and R = (X * Y) mod Z

   -- Scaled divide
   procedure Scaled_Divide
     (X, Y, Z : Int64;
      Q, R    : out Int64;
      Round   : Boolean);
   -- Computes Q = (X * Y) / Z with optional rounding

end System.Arith_64;
