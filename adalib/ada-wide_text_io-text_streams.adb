-- Ada.Wide_Text_IO.Text_Streams body for Z80
-- Stream access implementation

package body Ada.Wide_Text_IO.Text_Streams is

   -- Simple stream wrapper for wide text I/O
   type Wide_Text_Stream is new Ada.Streams.Root_Stream_Type with record
      File : File_Type;
   end record;

   overriding procedure Read
     (Stream : in Out Wide_Text_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in Out Wide_Text_Stream;
      Item   : Ada.Streams.Stream_Element_Array);

   The_Stream : aliased Wide_Text_Stream;

   overriding procedure Read
     (Stream : in Out Wide_Text_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      pragma Unreferenced (Stream);
   begin
      Last := Item'First - 1;
      -- Simplified: read bytes from wide text file
      for I in Item'Range loop
         declare
            WC : Wide_Character;
         begin
            Get (Stream.File, WC);
            Item (I) := Ada.Streams.Stream_Element (Wide_Character'Pos (WC) mod 256);
            Last := I;
         exception
            when End_Error =>
               exit;
         end;
      end loop;
   end Read;

   overriding procedure Write
     (Stream : in Out Wide_Text_Stream;
      Item   : Ada.Streams.Stream_Element_Array)
   is
      pragma Unreferenced (Stream);
   begin
      for E of Item loop
         Put (Stream.File, Wide_Character'Val (Natural (E)));
      end loop;
   end Write;

   ------------
   -- Stream --
   ------------

   function Stream (File : File_Type) return Stream_Access is
   begin
      The_Stream.File := File;
      return The_Stream'Access;
   end Stream;

end Ada.Wide_Text_IO.Text_Streams;
