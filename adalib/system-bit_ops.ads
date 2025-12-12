-- System.Bit_Ops for Z80
-- Low-level bit operations

package System.Bit_Ops is
   pragma Preelaborate;

   -- Bit operations on unsigned types

   function Bit_And (Left, Right : Unsigned_8) return Unsigned_8;
   pragma Import (Intrinsic, Bit_And);

   function Bit_Or (Left, Right : Unsigned_8) return Unsigned_8;
   pragma Import (Intrinsic, Bit_Or);

   function Bit_Xor (Left, Right : Unsigned_8) return Unsigned_8;
   pragma Import (Intrinsic, Bit_Xor);

   function Bit_Not (Value : Unsigned_8) return Unsigned_8;
   pragma Import (Intrinsic, Bit_Not);

   function Bit_And (Left, Right : Unsigned_16) return Unsigned_16;
   pragma Import (Intrinsic, Bit_And);

   function Bit_Or (Left, Right : Unsigned_16) return Unsigned_16;
   pragma Import (Intrinsic, Bit_Or);

   function Bit_Xor (Left, Right : Unsigned_16) return Unsigned_16;
   pragma Import (Intrinsic, Bit_Xor);

   function Bit_Not (Value : Unsigned_16) return Unsigned_16;
   pragma Import (Intrinsic, Bit_Not);

   -- Shift operations
   function Shift_Left (Value : Unsigned_8; Amount : Natural) return Unsigned_8;
   pragma Import (Intrinsic, Shift_Left);

   function Shift_Right (Value : Unsigned_8; Amount : Natural) return Unsigned_8;
   pragma Import (Intrinsic, Shift_Right);

   function Shift_Right_Arithmetic (Value : Unsigned_8; Amount : Natural) return Unsigned_8;
   pragma Import (Intrinsic, Shift_Right_Arithmetic);

   function Rotate_Left (Value : Unsigned_8; Amount : Natural) return Unsigned_8;
   pragma Import (Intrinsic, Rotate_Left);

   function Rotate_Right (Value : Unsigned_8; Amount : Natural) return Unsigned_8;
   pragma Import (Intrinsic, Rotate_Right);

   function Shift_Left (Value : Unsigned_16; Amount : Natural) return Unsigned_16;
   pragma Import (Intrinsic, Shift_Left);

   function Shift_Right (Value : Unsigned_16; Amount : Natural) return Unsigned_16;
   pragma Import (Intrinsic, Shift_Right);

   function Shift_Right_Arithmetic (Value : Unsigned_16; Amount : Natural) return Unsigned_16;
   pragma Import (Intrinsic, Shift_Right_Arithmetic);

   function Rotate_Left (Value : Unsigned_16; Amount : Natural) return Unsigned_16;
   pragma Import (Intrinsic, Rotate_Left);

   function Rotate_Right (Value : Unsigned_16; Amount : Natural) return Unsigned_16;
   pragma Import (Intrinsic, Rotate_Right);

private
   type Unsigned_8 is mod 2**8;
   type Unsigned_16 is mod 2**16;
end System.Bit_Ops;
