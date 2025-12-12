-- GNAT.String_Stream body for Z80
-- String-based stream I/O implementation

package body GNAT.String_Stream is

   use Ada.Streams;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in Out String_Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Idx : Stream_Element_Offset := Item'First;
   begin
      Last := Item'First - 1;

      while Idx <= Item'Last and Stream.Position <= Stream.Last_Pos loop
         Item (Idx) := Stream_Element (Character'Pos (Stream.Data (Stream.Position)));
         Stream.Position := Stream.Position + 1;
         Last := Idx;
         Idx := Idx + 1;
      end loop;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in Out String_Stream_Type;
      Item   : Stream_Element_Array)
   is
   begin
      for E of Item loop
         if Stream.Last_Pos < Stream.Max_Length then
            Stream.Last_Pos := Stream.Last_Pos + 1;
            Stream.Data (Stream.Last_Pos) := Character'Val (Integer (E) mod 256);
         end if;
      end loop;
   end Write;

   -----------
   -- Reset --
   -----------

   procedure Reset (Stream : in Out String_Stream_Type) is
   begin
      Stream.Position := 1;
   end Reset;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String
     (Stream : in Out String_Stream_Type;
      Data   : String)
   is
      Len : constant Natural := Natural'Min (Data'Length, Stream.Max_Length);
   begin
      Stream.Data (1 .. Len) := Data (Data'First .. Data'First + Len - 1);
      Stream.Last_Pos := Len;
      Stream.Position := 1;
   end Set_String;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Stream : String_Stream_Type) return String is
   begin
      return Stream.Data (1 .. Stream.Last_Pos);
   end Get_String;

   ------------
   -- Length --
   ------------

   function Length (Stream : String_Stream_Type) return Natural is
   begin
      return Stream.Last_Pos;
   end Length;

end GNAT.String_Stream;
