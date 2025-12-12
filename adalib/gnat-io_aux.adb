-- GNAT.IO_Aux body for Z80
-- Auxiliary I/O implementation

with Ada.Text_IO;
with GNAT.OS_Lib;

package body GNAT.IO_Aux is

   -----------------
   -- File_Exists --
   -----------------

   function File_Exists (Name : String) return Boolean is
   begin
      return GNAT.OS_Lib.Is_Regular_File (Name);
   end File_Exists;

   --------------
   -- Get_Line --
   --------------

   function Get_Line return String is
      Buffer : String (1 .. 256);
      Last   : Natural;
   begin
      Ada.Text_IO.Get_Line (Buffer, Last);
      return Buffer (1 .. Last);
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (Item : out String; Last : out Natural) is
   begin
      Ada.Text_IO.Get_Line (Item, Last);
   end Get_Line;

end GNAT.IO_Aux;
