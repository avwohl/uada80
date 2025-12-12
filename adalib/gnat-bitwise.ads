-- GNAT.Bitwise for Z80
-- Bitwise operations for various types

with System.Unsigned_Types;

package GNAT.Bitwise is
   pragma Pure;

   use System.Unsigned_Types;

   -- Bit operations
   function Count_Ones (Value : Unsigned) return Natural;
   --  Count number of 1 bits

   function Count_Zeros (Value : Unsigned) return Natural;
   --  Count number of 0 bits

   function Leading_Zeros (Value : Unsigned) return Natural;
   --  Count leading zero bits

   function Trailing_Zeros (Value : Unsigned) return Natural;
   --  Count trailing zero bits

   function Leading_Ones (Value : Unsigned) return Natural;
   --  Count leading one bits

   function Trailing_Ones (Value : Unsigned) return Natural;
   --  Count trailing one bits

   function Rotate_Left (Value : Unsigned; Amount : Natural) return Unsigned;
   --  Rotate bits left

   function Rotate_Right (Value : Unsigned; Amount : Natural) return Unsigned;
   --  Rotate bits right

   function Reverse_Bits (Value : Unsigned) return Unsigned;
   --  Reverse bit order

   function Is_Power_Of_Two (Value : Unsigned) return Boolean;
   --  Check if value is a power of 2

   function Next_Power_Of_Two (Value : Unsigned) return Unsigned;
   --  Return next power of 2 >= Value

end GNAT.Bitwise;
