-- Ada.Strings.Text_Buffers.Unbounded body for Z80
-- Unbounded text buffer implementation

package body Ada.Strings.Text_Buffers.Unbounded is

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Buffer : in Out Buffer_Type;
      Item   : String)
   is
      Avail : constant Natural := Max_Buffer_Size - Buffer.Length;
      Len   : constant Natural := Natural'Min (Item'Length, Avail);
   begin
      if Len > 0 then
         Buffer.Data (Buffer.Length + 1 .. Buffer.Length + Len) :=
           Item (Item'First .. Item'First + Len - 1);
         Buffer.Length := Buffer.Length + Len;
      end if;
   end Put;

   --------------
   -- Wide_Put --
   --------------

   overriding procedure Wide_Put
     (Buffer : in Out Buffer_Type;
      Item   : Wide_String)
   is
   begin
      -- Convert to narrow string (simplified)
      for WC of Item loop
         if Wide_Character'Pos (WC) < 256 then
            Put (Buffer, (1 => Character'Val (Wide_Character'Pos (WC))));
         else
            Put (Buffer, "?");
         end if;
      end loop;
   end Wide_Put;

   -------------------
   -- Wide_Wide_Put --
   -------------------

   overriding procedure Wide_Wide_Put
     (Buffer : in Out Buffer_Type;
      Item   : Wide_Wide_String)
   is
   begin
      for WWC of Item loop
         if Wide_Wide_Character'Pos (WWC) < 256 then
            Put (Buffer, (1 => Character'Val (Wide_Wide_Character'Pos (WWC))));
         else
            Put (Buffer, "?");
         end if;
      end loop;
   end Wide_Wide_Put;

   --------------
   -- Put_UTF_8 --
   --------------

   overriding procedure Put_UTF_8
     (Buffer : in Out Buffer_Type;
      Item   : String)
   is
   begin
      Put (Buffer, Item);
   end Put_UTF_8;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line
     (Buffer : in Out Buffer_Type)
   is
   begin
      Put (Buffer, (1 => ASCII.LF));
   end New_Line;

   --------------------
   -- Current_Indent --
   --------------------

   overriding function Current_Indent
     (Buffer : Buffer_Type) return Text_Buffer_Count
   is
   begin
      return Buffer.Indent_Level;
   end Current_Indent;

   ---------------------
   -- Increase_Indent --
   ---------------------

   overriding procedure Increase_Indent
     (Buffer : in Out Buffer_Type;
      Amount : Text_Buffer_Count := 1)
   is
   begin
      Buffer.Indent_Level := Buffer.Indent_Level + Amount;
   end Increase_Indent;

   ---------------------
   -- Decrease_Indent --
   ---------------------

   overriding procedure Decrease_Indent
     (Buffer : in Out Buffer_Type;
      Amount : Text_Buffer_Count := 1)
   is
   begin
      if Buffer.Indent_Level >= Amount then
         Buffer.Indent_Level := Buffer.Indent_Level - Amount;
      else
         Buffer.Indent_Level := 0;
      end if;
   end Decrease_Indent;

   ---------
   -- Get --
   ---------

   function Get (Buffer : Buffer_Type) return String is
   begin
      return Buffer.Data (1 .. Buffer.Length);
   end Get;

   -----------
   -- Clear --
   -----------

   procedure Clear (Buffer : in Out Buffer_Type) is
   begin
      Buffer.Length := 0;
      Buffer.Indent_Level := 0;
   end Clear;

end Ada.Strings.Text_Buffers.Unbounded;
