-- Ada.Streams.Storage body for Z80
-- Storage stream support implementation

package body Ada.Streams.Storage is

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in Out Storage_Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Available : Stream_Element_Count;
      To_Read   : Stream_Element_Count;
   begin
      Available := Stream.Write_Pos - Stream.Read_Pos;
      To_Read := Stream_Element_Count'Min (Item'Length, Available);

      if To_Read > 0 then
         for I in 0 .. To_Read - 1 loop
            Item (Item'First + I) := Stream.Data (Stream.Read_Pos + I);
         end loop;
         Stream.Read_Pos := Stream.Read_Pos + To_Read;
         Last := Item'First + To_Read - 1;
      else
         Last := Item'First - 1;
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in Out Storage_Stream_Type;
      Item   : Stream_Element_Array)
   is
      Available : Stream_Element_Count;
      To_Write  : Stream_Element_Count;
   begin
      Available := Stream.Size - Stream.Write_Pos + 1;
      To_Write := Stream_Element_Count'Min (Item'Length, Available);

      if To_Write > 0 then
         for I in 0 .. To_Write - 1 loop
            Stream.Data (Stream.Write_Pos + I) := Item (Item'First + I);
         end loop;
         Stream.Write_Pos := Stream.Write_Pos + To_Write;
      end if;

      -- Raise error if buffer overflow
      if To_Write < Item'Length then
         raise Constraint_Error with "Storage stream overflow";
      end if;
   end Write;

   -----------
   -- Reset --
   -----------

   procedure Reset (Stream : in Out Storage_Stream_Type) is
   begin
      Stream.Read_Pos := 1;
   end Reset;

   --------------
   -- Position --
   --------------

   function Position (Stream : Storage_Stream_Type) return Stream_Element_Count is
   begin
      return Stream.Read_Pos - 1;
   end Position;

   ------------
   -- Length --
   ------------

   function Length (Stream : Storage_Stream_Type) return Stream_Element_Count is
   begin
      return Stream.Write_Pos - 1;
   end Length;

   -------------------
   -- End_Of_Stream --
   -------------------

   function End_Of_Stream (Stream : Storage_Stream_Type) return Boolean is
   begin
      return Stream.Read_Pos >= Stream.Write_Pos;
   end End_Of_Stream;

   -------------
   -- Storage --
   -------------

   function Storage (Stream : Storage_Stream_Type)
     return Stream_Element_Array
   is
   begin
      return Stream.Data (1 .. Stream.Write_Pos - 1);
   end Storage;

end Ada.Streams.Storage;
