-- Ada.Text_IO.Unbounded_IO for Z80
-- Text I/O for Unbounded_String type

with Ada.Strings.Unbounded;

package Ada.Text_IO.Unbounded_IO is

   procedure Put
     (File : File_Type;
      Item : Ada.Strings.Unbounded.Unbounded_String);

   procedure Put
     (Item : Ada.Strings.Unbounded.Unbounded_String);

   procedure Put_Line
     (File : File_Type;
      Item : Ada.Strings.Unbounded.Unbounded_String);

   procedure Put_Line
     (Item : Ada.Strings.Unbounded.Unbounded_String);

   function Get_Line
     (File : File_Type)
      return Ada.Strings.Unbounded.Unbounded_String;

   function Get_Line
      return Ada.Strings.Unbounded.Unbounded_String;

   procedure Get_Line
     (File : File_Type;
      Item : out Ada.Strings.Unbounded.Unbounded_String);

   procedure Get_Line
     (Item : out Ada.Strings.Unbounded.Unbounded_String);

end Ada.Text_IO.Unbounded_IO;
