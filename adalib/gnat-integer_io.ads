-- GNAT.Integer_IO for Z80
-- Simple integer I/O

package GNAT.Integer_IO is
   pragma Preelaborate;

   procedure Put (Item : Integer; Width : Natural := 0; Base : Positive := 10);
   --  Output integer with optional width and base

   procedure Get (Item : out Integer);
   --  Read integer from input

   function Image (Value : Integer; Base : Positive := 10) return String;
   --  Return string representation of integer

   function Value (S : String; Base : Positive := 10) return Integer;
   --  Parse string as integer

end GNAT.Integer_IO;
