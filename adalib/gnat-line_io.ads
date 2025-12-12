-- GNAT.Line_IO for Z80
-- Line-oriented I/O

package GNAT.Line_IO is
   pragma Preelaborate;

   Max_Line_Length : constant := 255;
   --  Maximum line length for Z80

   type Line_Buffer is limited private;
   --  A line buffer

   procedure Initialize (Buffer : out Line_Buffer);
   --  Initialize a line buffer

   procedure Read_Line
     (Buffer : in Out Line_Buffer;
      S      : out String;
      Last   : out Natural);
   --  Read a line into buffer

   procedure Write_Line (S : String);
   --  Write a line to output

   procedure Write_Line;
   --  Write an empty line

   function Get_Line return String;
   --  Read and return a line

private

   type Line_Buffer is record
      Data   : String (1 .. Max_Line_Length);
      Length : Natural := 0;
   end record;

end GNAT.Line_IO;
