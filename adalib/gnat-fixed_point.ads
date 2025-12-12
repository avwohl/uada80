-- GNAT.Fixed_Point for Z80
-- Fixed-point arithmetic utilities

package GNAT.Fixed_Point is
   pragma Pure;

   -- 16.16 fixed point for Z80
   type Fixed_16_16 is delta 1.0 / 65536.0 range -32768.0 .. 32767.999984741;
   for Fixed_16_16'Size use 32;

   -- 8.8 fixed point (more efficient for Z80)
   type Fixed_8_8 is delta 1.0 / 256.0 range -128.0 .. 127.99609375;
   for Fixed_8_8'Size use 16;

   function To_Fixed_16_16 (Value : Integer) return Fixed_16_16;
   function To_Fixed_16_16 (Value : Float) return Fixed_16_16;
   function To_Integer (Value : Fixed_16_16) return Integer;
   function To_Float (Value : Fixed_16_16) return Float;

   function To_Fixed_8_8 (Value : Integer) return Fixed_8_8;
   function To_Fixed_8_8 (Value : Float) return Fixed_8_8;
   function To_Integer (Value : Fixed_8_8) return Integer;
   function To_Float (Value : Fixed_8_8) return Float;

   -- Fast multiply/divide for fixed point
   function Multiply (A, B : Fixed_8_8) return Fixed_8_8;
   function Divide (A, B : Fixed_8_8) return Fixed_8_8;

end GNAT.Fixed_Point;
