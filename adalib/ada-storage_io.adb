-- Ada.Storage_IO body for Z80
-- In-memory streaming I/O implementation

with System;

package body Ada.Storage_IO is

   ----------
   -- Read --
   ----------

   procedure Read (Buffer : Buffer_Type; Item : out Element_Type) is
      -- Simple byte-by-byte copy from buffer to item
      Item_Addr : System.Address := Item'Address;
      Buf_Idx   : Ada.Streams.Stream_Element_Offset := Buffer'First;
   begin
      -- Copy buffer to item storage
      -- On Z80, this could be optimized with LDIR
      for I in 1 .. Buffer_Size loop
         declare
            Byte_Ptr : access Ada.Streams.Stream_Element;
            for Byte_Ptr'Address use Item_Addr + System.Storage_Offset (I - 1);
         begin
            Byte_Ptr.all := Buffer (Buf_Idx);
         end;
         Buf_Idx := Buf_Idx + 1;
      end loop;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (Buffer : out Buffer_Type; Item : Element_Type) is
      Item_Addr : System.Address := Item'Address;
      Buf_Idx   : Ada.Streams.Stream_Element_Offset := Buffer'First;
   begin
      -- Copy item storage to buffer
      for I in 1 .. Buffer_Size loop
         declare
            Byte_Ptr : access constant Ada.Streams.Stream_Element;
            for Byte_Ptr'Address use Item_Addr + System.Storage_Offset (I - 1);
         begin
            Buffer (Buf_Idx) := Byte_Ptr.all;
         end;
         Buf_Idx := Buf_Idx + 1;
      end loop;
   end Write;

end Ada.Storage_IO;
