-- Ada.Wide_Wide_Text_IO.Text_Streams body for Z80
-- Stream access implementation

package body Ada.Wide_Wide_Text_IO.Text_Streams is

   type WWText_Stream is new Ada.Streams.Root_Stream_Type with record
      File : File_Type;
   end record;

   overriding procedure Read
     (Stream : in Out WWText_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in Out WWText_Stream;
      Item   : Ada.Streams.Stream_Element_Array);

   The_Stream : aliased WWText_Stream;

   overriding procedure Read
     (Stream : in Out WWText_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      pragma Unreferenced (Stream);
   begin
      Last := Item'First - 1;
      for I in Item'Range loop
         declare
            WWC : Wide_Wide_Character;
         begin
            Get (Stream.File, WWC);
            Item (I) := Ada.Streams.Stream_Element (Wide_Wide_Character'Pos (WWC) mod 256);
            Last := I;
         exception
            when End_Error =>
               exit;
         end;
      end loop;
   end Read;

   overriding procedure Write
     (Stream : in Out WWText_Stream;
      Item   : Ada.Streams.Stream_Element_Array)
   is
      pragma Unreferenced (Stream);
   begin
      for E of Item loop
         Put (Stream.File, Wide_Wide_Character'Val (Natural (E)));
      end loop;
   end Write;

   function Stream (File : File_Type) return Stream_Access is
   begin
      The_Stream.File := File;
      return The_Stream'Access;
   end Stream;

end Ada.Wide_Wide_Text_IO.Text_Streams;
