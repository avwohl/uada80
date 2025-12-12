-- Ada.Streams package for Z80
-- Provides base stream abstraction for I/O operations
--
-- This is a simplified implementation for Z80 with limited resources.
-- Stream_Element is a single byte (8 bits).

package Ada.Streams is
   pragma Preelaborate;

   -- Stream_Element is an 8-bit byte
   type Stream_Element is mod 2**8;

   -- Stream_Element_Offset for positioning (16-bit for Z80)
   type Stream_Element_Offset is range -32768 .. 32767;

   subtype Stream_Element_Count is
     Stream_Element_Offset range 0 .. Stream_Element_Offset'Last;

   -- Stream_Element_Array is an array of bytes
   type Stream_Element_Array is
     array (Stream_Element_Offset range <>) of aliased Stream_Element;

   -- Root_Stream_Type is the base class for all streams
   type Root_Stream_Type is abstract tagged limited private;

   -- Read operation - must be overridden
   procedure Read
     (Stream : in out Root_Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is abstract;

   -- Write operation - must be overridden
   procedure Write
     (Stream : in Out Root_Stream_Type;
      Item   : Stream_Element_Array) is abstract;

private

   type Root_Stream_Type is abstract tagged limited null record;

end Ada.Streams;
