-- System.Float_Rep for Z80
-- Floating-point representation utilities

package System.Float_Rep is
   pragma Pure;

   -- Float is 32-bit IEEE single precision
   -- Long_Float is 48-bit z88dk format

   type Unsigned_8 is mod 2 ** 8;
   type Unsigned_16 is mod 2 ** 16;
   type Unsigned_32 is mod 2 ** 32;

   -- IEEE 32-bit float components
   type Float_Components is record
      Sign     : Boolean;
      Exponent : Integer;
      Mantissa : Unsigned_32;
   end record;

   -- 48-bit float components
   type Long_Float_Components is record
      Sign     : Boolean;
      Exponent : Integer;
      Mantissa_Hi : Unsigned_32;
      Mantissa_Lo : Unsigned_16;
   end record;

   -- Decompose Float
   function Decompose (F : Float) return Float_Components;

   -- Compose Float
   function Compose (C : Float_Components) return Float;

   -- Get sign bit
   function Sign_Bit (F : Float) return Boolean;

   -- Get exponent
   function Get_Exponent (F : Float) return Integer;

   -- Get mantissa
   function Get_Mantissa (F : Float) return Unsigned_32;

   -- Check for zero
   function Is_Zero (F : Float) return Boolean;

   -- Check for infinity (not really supported on Z80)
   function Is_Infinite (F : Float) return Boolean;

   -- Check for NaN (not really supported on Z80)
   function Is_NaN (F : Float) return Boolean;

end System.Float_Rep;
