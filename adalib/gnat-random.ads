-- GNAT.Random for Z80
-- Simple random number generation

package GNAT.Random is
   pragma Preelaborate;

   type Generator is limited private;
   --  Random number generator state

   procedure Reset (Gen : out Generator);
   --  Reset generator to default state

   procedure Reset (Gen : out Generator; Seed : Integer);
   --  Reset generator with given seed

   function Random (Gen : Generator) return Float;
   --  Return random Float in [0.0, 1.0)

   function Random_Integer
     (Gen  : Generator;
      Low  : Integer;
      High : Integer) return Integer;
   --  Return random Integer in [Low, High]

   function Random_Boolean (Gen : Generator) return Boolean;
   --  Return random Boolean

private

   type Generator is record
      State : Integer := 12345;  -- LFSR seed
   end record;

end GNAT.Random;
