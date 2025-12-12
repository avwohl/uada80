-- GNAT.String_Stream for Z80
-- String-based stream I/O

with Ada.Streams;

package GNAT.String_Stream is
   pragma Preelaborate;

   type String_Stream_Type (Max_Length : Positive) is
     new Ada.Streams.Root_Stream_Type with private;

   overriding procedure Read
     (Stream : in Out String_Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in Out String_Stream_Type;
      Item   : Ada.Streams.Stream_Element_Array);

   procedure Reset (Stream : in Out String_Stream_Type);
   --  Reset stream to beginning

   procedure Set_String
     (Stream : in Out String_Stream_Type;
      Data   : String);
   --  Set stream contents from string

   function Get_String (Stream : String_Stream_Type) return String;
   --  Get stream contents as string

   function Length (Stream : String_Stream_Type) return Natural;
   --  Return current content length

private

   type String_Stream_Type (Max_Length : Positive) is
     new Ada.Streams.Root_Stream_Type with record
      Data     : String (1 .. Max_Length) := (others => ' ');
      Position : Natural := 1;
      Last_Pos : Natural := 0;
   end record;

end GNAT.String_Stream;
