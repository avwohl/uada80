-- GNAT.Float_IO for Z80
-- Simple float I/O

package GNAT.Float_IO is
   pragma Preelaborate;

   procedure Put
     (Item : Float;
      Fore : Natural := 2;
      Aft  : Natural := 6;
      Exp  : Natural := 0);
   --  Output float

   procedure Get (Item : out Float);
   --  Read float from input

   function Image
     (Value : Float;
      Aft   : Natural := 6) return String;
   --  Return string representation of float

   function Value (S : String) return Float;
   --  Parse string as float

end GNAT.Float_IO;
