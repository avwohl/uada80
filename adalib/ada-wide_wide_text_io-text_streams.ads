-- Ada.Wide_Wide_Text_IO.Text_Streams for Z80
-- Stream access for wide wide text files

with Ada.Streams;

package Ada.Wide_Wide_Text_IO.Text_Streams is

   type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;

   function Stream (File : File_Type) return Stream_Access;
   --  Return stream for wide wide text file

end Ada.Wide_Wide_Text_IO.Text_Streams;
