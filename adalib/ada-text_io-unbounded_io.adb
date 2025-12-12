-- Ada.Text_IO.Unbounded_IO body for Z80
-- Text I/O for Unbounded_String implementation

package body Ada.Text_IO.Unbounded_IO is

   use Ada.Strings.Unbounded;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : File_Type;
      Item : Unbounded_String)
   is
   begin
      Put (File, To_String (Item));
   end Put;

   procedure Put
     (Item : Unbounded_String)
   is
   begin
      Put (To_String (Item));
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (File : File_Type;
      Item : Unbounded_String)
   is
   begin
      Put_Line (File, To_String (Item));
   end Put_Line;

   procedure Put_Line
     (Item : Unbounded_String)
   is
   begin
      Put_Line (To_String (Item));
   end Put_Line;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (File : File_Type)
      return Unbounded_String
   is
      Buffer : String (1 .. 255);
      Last   : Natural;
   begin
      Get_Line (File, Buffer, Last);
      return To_Unbounded_String (Buffer (1 .. Last));
   end Get_Line;

   function Get_Line
      return Unbounded_String
   is
   begin
      return Get_Line (Current_Input);
   end Get_Line;

   procedure Get_Line
     (File : File_Type;
      Item : out Unbounded_String)
   is
   begin
      Item := Get_Line (File);
   end Get_Line;

   procedure Get_Line
     (Item : out Unbounded_String)
   is
   begin
      Item := Get_Line;
   end Get_Line;

end Ada.Text_IO.Unbounded_IO;
