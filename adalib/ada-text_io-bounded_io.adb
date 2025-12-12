-- Ada.Text_IO.Bounded_IO body for Z80
-- Text I/O for Bounded_String implementation

package body Ada.Text_IO.Bounded_IO is

   use Bounded;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : File_Type;
      Item : Bounded_String)
   is
   begin
      Put (File, To_String (Item));
   end Put;

   procedure Put
     (Item : Bounded_String)
   is
   begin
      Put (To_String (Item));
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (File : File_Type;
      Item : Bounded_String)
   is
   begin
      Put_Line (File, To_String (Item));
   end Put_Line;

   procedure Put_Line
     (Item : Bounded_String)
   is
   begin
      Put_Line (To_String (Item));
   end Put_Line;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (File : File_Type)
      return Bounded_String
   is
      Buffer : String (1 .. Max_Length);
      Last   : Natural;
   begin
      Get_Line (File, Buffer, Last);
      return To_Bounded_String (Buffer (1 .. Last));
   end Get_Line;

   function Get_Line
      return Bounded_String
   is
   begin
      return Get_Line (Current_Input);
   end Get_Line;

   procedure Get_Line
     (File : File_Type;
      Item : out Bounded_String)
   is
   begin
      Item := Get_Line (File);
   end Get_Line;

   procedure Get_Line
     (Item : out Bounded_String)
   is
   begin
      Item := Get_Line;
   end Get_Line;

end Ada.Text_IO.Bounded_IO;
