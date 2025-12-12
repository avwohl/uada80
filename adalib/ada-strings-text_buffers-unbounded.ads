-- Ada.Strings.Text_Buffers.Unbounded for Z80
-- Unbounded text buffer implementation

with Ada.Strings.Text_Buffers;

package Ada.Strings.Text_Buffers.Unbounded is
   pragma Preelaborate;

   type Buffer_Type is new Root_Buffer_Type with private;

   overriding procedure Put
     (Buffer : in Out Buffer_Type;
      Item   : String);

   overriding procedure Wide_Put
     (Buffer : in Out Buffer_Type;
      Item   : Wide_String);

   overriding procedure Wide_Wide_Put
     (Buffer : in Out Buffer_Type;
      Item   : Wide_Wide_String);

   overriding procedure Put_UTF_8
     (Buffer : in Out Buffer_Type;
      Item   : String);

   overriding procedure New_Line
     (Buffer : in Out Buffer_Type);

   overriding function Current_Indent
     (Buffer : Buffer_Type) return Text_Buffer_Count;

   overriding procedure Increase_Indent
     (Buffer : in Out Buffer_Type;
      Amount : Text_Buffer_Count := 1);

   overriding procedure Decrease_Indent
     (Buffer : in Out Buffer_Type;
      Amount : Text_Buffer_Count := 1);

   function Get (Buffer : Buffer_Type) return String;
   --  Get accumulated text

   procedure Clear (Buffer : in Out Buffer_Type);
   --  Clear buffer contents

private
   Max_Buffer_Size : constant := 4096;

   type Buffer_Type is new Root_Buffer_Type with record
      Data   : String (1 .. Max_Buffer_Size);
      Length : Natural := 0;
   end record;

end Ada.Strings.Text_Buffers.Unbounded;
