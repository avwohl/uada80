-- Ada.Storage_IO for Z80
-- In-memory streaming I/O
--
-- Provides I/O to/from memory buffers

with Ada.Streams;

generic
   type Element_Type is private;
package Ada.Storage_IO is
   pragma Preelaborate;

   Buffer_Size : constant Ada.Streams.Stream_Element_Count :=
     (Element_Type'Size + 7) / 8;
   -- Size needed in stream elements (bytes)

   subtype Buffer_Type is
     Ada.Streams.Stream_Element_Array (1 .. Buffer_Size);

   procedure Read (Buffer : Buffer_Type; Item : out Element_Type);
   -- Read Item from Buffer

   procedure Write (Buffer : out Buffer_Type; Item : Element_Type);
   -- Write Item to Buffer

end Ada.Storage_IO;
