-- GNAT.Fixed_Point body for Z80
-- Fixed-point arithmetic utilities implementation

package body GNAT.Fixed_Point is

   ---------------------
   -- To_Fixed_16_16 --
   ---------------------

   function To_Fixed_16_16 (Value : Integer) return Fixed_16_16 is
   begin
      return Fixed_16_16 (Value);
   end To_Fixed_16_16;

   function To_Fixed_16_16 (Value : Float) return Fixed_16_16 is
   begin
      return Fixed_16_16 (Value);
   end To_Fixed_16_16;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Value : Fixed_16_16) return Integer is
   begin
      return Integer (Value);
   end To_Integer;

   --------------
   -- To_Float --
   --------------

   function To_Float (Value : Fixed_16_16) return Float is
   begin
      return Float (Value);
   end To_Float;

   -------------------
   -- To_Fixed_8_8 --
   -------------------

   function To_Fixed_8_8 (Value : Integer) return Fixed_8_8 is
   begin
      return Fixed_8_8 (Value);
   end To_Fixed_8_8;

   function To_Fixed_8_8 (Value : Float) return Fixed_8_8 is
   begin
      return Fixed_8_8 (Value);
   end To_Fixed_8_8;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Value : Fixed_8_8) return Integer is
   begin
      return Integer (Value);
   end To_Integer;

   --------------
   -- To_Float --
   --------------

   function To_Float (Value : Fixed_8_8) return Float is
   begin
      return Float (Value);
   end To_Float;

   --------------
   -- Multiply --
   --------------

   function Multiply (A, B : Fixed_8_8) return Fixed_8_8 is
   begin
      return A * B;
   end Multiply;

   ------------
   -- Divide --
   ------------

   function Divide (A, B : Fixed_8_8) return Fixed_8_8 is
   begin
      return A / B;
   end Divide;

end GNAT.Fixed_Point;
