-- System.Arith_32 for Z80
-- 32-bit arithmetic operations with overflow checking

package System.Arith_32 is
   pragma Pure;

   type Int32 is range -2**31 .. 2**31 - 1;
   type Uns32 is mod 2**32;

   -- Addition with overflow check
   function Add_With_Ovflo_Check (X, Y : Int32) return Int32;

   -- Subtraction with overflow check
   function Subtract_With_Ovflo_Check (X, Y : Int32) return Int32;

   -- Multiplication with overflow check
   function Multiply_With_Ovflo_Check (X, Y : Int32) return Int32;

   -- Division (quotient and remainder)
   procedure Divide
     (X, Y : Int32;
      Q, R : out Int32);

   -- Scaled division: (X * Y) / Z
   procedure Scaled_Divide
     (X, Y, Z : Int32;
      Q, R    : out Int32;
      Round   : Boolean);

   -- Double divide: computes quotient and remainder of (X * Y) / Z
   procedure Double_Divide
     (X, Y, Z : Int32;
      Q, R    : out Int32);

end System.Arith_32;
