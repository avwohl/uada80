-- GNAT.MBBS_Float_Random for Z80
-- Mersenne Twister based float random numbers

package GNAT.MBBS_Float_Random is

   type Generator is limited private;

   subtype Uniformly_Distributed is Float range 0.0 .. 1.0;

   function Random (Gen : Generator) return Uniformly_Distributed;
   --  Return random value in [0.0, 1.0)

   procedure Reset (Gen : in Out Generator);
   --  Reset generator with time-based seed

   procedure Reset (Gen : in Out Generator; Initiator : Integer);
   --  Reset generator with specific seed

   function Image (Of_State : Generator) return String;
   --  Return state as string

   function Value (Coded_State : String) return Generator;
   --  Restore state from string

   Max_Image_Width : constant := 32;

private
   type Generator is record
      State : Natural := 12345;
   end record;

end GNAT.MBBS_Float_Random;
