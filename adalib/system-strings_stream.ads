-- System.Strings_Stream for Z80
-- String-based stream implementation

with Ada.Streams;

package System.Strings_Stream is

   use Ada.Streams;

   -- Maximum buffer size for string streams
   Max_Buffer_Size : constant := 256;

   -- String-based stream type
   type String_Stream is new Root_Stream_Type with private;

   -- Stream operations
   procedure Read
     (Stream : in Out String_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   procedure Write
     (Stream : in Out String_Stream;
      Item   : Stream_Element_Array);

   -- String-specific operations
   procedure Set_String (Stream : in Out String_Stream; S : String);
   function Get_String (Stream : String_Stream) return String;

   -- Reset stream position
   procedure Reset (Stream : in Out String_Stream);

   -- Get current length
   function Length (Stream : String_Stream) return Natural;

private
   type String_Stream is new Root_Stream_Type with record
      Buffer   : String (1 .. Max_Buffer_Size);
      Last     : Natural := 0;
      Position : Natural := 1;
   end record;

end System.Strings_Stream;
