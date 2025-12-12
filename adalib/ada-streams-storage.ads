-- Ada.Streams.Storage for Z80
-- Storage stream support

with Ada.Streams;
with System.Storage_Elements;

package Ada.Streams.Storage is
   pragma Preelaborate;

   -- Storage stream type
   type Storage_Stream_Type (Size : Stream_Element_Count) is
     new Root_Stream_Type with private;

   -- Stream operations
   overriding procedure Read
     (Stream : in Out Storage_Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding procedure Write
     (Stream : in Out Storage_Stream_Type;
      Item   : Stream_Element_Array);

   -- Reset stream to beginning
   procedure Reset (Stream : in Out Storage_Stream_Type);

   -- Get current position
   function Position (Stream : Storage_Stream_Type) return Stream_Element_Count;

   -- Get total data stored
   function Length (Stream : Storage_Stream_Type) return Stream_Element_Count;

   -- Check if at end
   function End_Of_Stream (Stream : Storage_Stream_Type) return Boolean;

   -- Get underlying storage
   function Storage (Stream : Storage_Stream_Type)
     return Stream_Element_Array;

private

   type Storage_Stream_Type (Size : Stream_Element_Count) is
     new Root_Stream_Type with record
      Data      : Stream_Element_Array (1 .. Size);
      Read_Pos  : Stream_Element_Offset := 1;
      Write_Pos : Stream_Element_Offset := 1;
   end record;

end Ada.Streams.Storage;
