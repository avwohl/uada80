-- GNAT.IO_Aux for Z80
-- Auxiliary I/O routines

package GNAT.IO_Aux is

   function File_Exists (Name : String) return Boolean;
   --  Check if file exists

   function Get_Line return String;
   --  Read line from standard input

   procedure Get_Line (Item : out String; Last : out Natural);
   --  Read line with explicit bounds

end GNAT.IO_Aux;
