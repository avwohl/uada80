-- Interfaces package for Z80
-- Provides standard integer types for interfacing with C and hardware

package Interfaces is
   pragma Pure;

   -- Signed integer types
   type Integer_8  is range -128 .. 127;
   for Integer_8'Size use 8;

   type Integer_16 is range -32768 .. 32767;
   for Integer_16'Size use 16;

   type Integer_32 is range -2147483648 .. 2147483647;
   for Integer_32'Size use 32;

   -- Unsigned integer types
   type Unsigned_8  is mod 2**8;
   for Unsigned_8'Size use 8;

   type Unsigned_16 is mod 2**16;
   for Unsigned_16'Size use 16;

   type Unsigned_32 is mod 2**32;
   for Unsigned_32'Size use 32;

   -- Shift and rotate operations for Unsigned_8
   function Shift_Left  (Value : Unsigned_8; Amount : Natural) return Unsigned_8;
   function Shift_Right (Value : Unsigned_8; Amount : Natural) return Unsigned_8;
   function Shift_Right_Arithmetic (Value : Unsigned_8; Amount : Natural) return Unsigned_8;
   function Rotate_Left  (Value : Unsigned_8; Amount : Natural) return Unsigned_8;
   function Rotate_Right (Value : Unsigned_8; Amount : Natural) return Unsigned_8;

   -- Shift and rotate operations for Unsigned_16
   function Shift_Left  (Value : Unsigned_16; Amount : Natural) return Unsigned_16;
   function Shift_Right (Value : Unsigned_16; Amount : Natural) return Unsigned_16;
   function Shift_Right_Arithmetic (Value : Unsigned_16; Amount : Natural) return Unsigned_16;
   function Rotate_Left  (Value : Unsigned_16; Amount : Natural) return Unsigned_16;
   function Rotate_Right (Value : Unsigned_16; Amount : Natural) return Unsigned_16;

   -- Shift and rotate operations for Unsigned_32
   function Shift_Left  (Value : Unsigned_32; Amount : Natural) return Unsigned_32;
   function Shift_Right (Value : Unsigned_32; Amount : Natural) return Unsigned_32;
   function Shift_Right_Arithmetic (Value : Unsigned_32; Amount : Natural) return Unsigned_32;
   function Rotate_Left  (Value : Unsigned_32; Amount : Natural) return Unsigned_32;
   function Rotate_Right (Value : Unsigned_32; Amount : Natural) return Unsigned_32;

   pragma Import (Intrinsic, Shift_Left);
   pragma Import (Intrinsic, Shift_Right);
   pragma Import (Intrinsic, Shift_Right_Arithmetic);
   pragma Import (Intrinsic, Rotate_Left);
   pragma Import (Intrinsic, Rotate_Right);

end Interfaces;
