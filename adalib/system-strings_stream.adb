-- System.Strings_Stream body for Z80
-- String-based stream implementation

package body System.Strings_Stream is

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in Out String_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Avail : Natural;
      Count : Natural;
   begin
      Avail := Stream.Last - Stream.Position + 1;
      Count := Natural'Min (Avail, Item'Length);

      if Count = 0 then
         Last := Item'First - 1;
         return;
      end if;

      for I in 0 .. Count - 1 loop
         Item (Item'First + Stream_Element_Offset (I)) :=
           Stream_Element (Character'Pos (Stream.Buffer (Stream.Position + I)));
      end loop;

      Stream.Position := Stream.Position + Count;
      Last := Item'First + Stream_Element_Offset (Count - 1);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in Out String_Stream;
      Item   : Stream_Element_Array)
   is
      Space : Natural;
      Count : Natural;
   begin
      Space := Max_Buffer_Size - Stream.Last;
      Count := Natural'Min (Space, Item'Length);

      for I in 0 .. Count - 1 loop
         Stream.Last := Stream.Last + 1;
         Stream.Buffer (Stream.Last) :=
           Character'Val (Natural (Item (Item'First + Stream_Element_Offset (I))));
      end loop;
   end Write;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String (Stream : in Out String_Stream; S : String) is
      Count : constant Natural := Natural'Min (S'Length, Max_Buffer_Size);
   begin
      Stream.Buffer (1 .. Count) := S (S'First .. S'First + Count - 1);
      Stream.Last := Count;
      Stream.Position := 1;
   end Set_String;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Stream : String_Stream) return String is
   begin
      return Stream.Buffer (1 .. Stream.Last);
   end Get_String;

   -----------
   -- Reset --
   -----------

   procedure Reset (Stream : in Out String_Stream) is
   begin
      Stream.Position := 1;
   end Reset;

   ------------
   -- Length --
   ------------

   function Length (Stream : String_Stream) return Natural is
   begin
      return Stream.Last;
   end Length;

end System.Strings_Stream;
