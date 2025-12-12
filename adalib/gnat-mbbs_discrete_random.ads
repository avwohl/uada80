-- GNAT.MBBS_Discrete_Random for Z80
-- Mersenne Twister based discrete random numbers

generic
   type Result_Subtype is (<>);
package GNAT.MBBS_Discrete_Random is

   type Generator is limited private;

   function Random (Gen : Generator) return Result_Subtype;
   --  Return random value in Result_Subtype'Range

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

end GNAT.MBBS_Discrete_Random;
