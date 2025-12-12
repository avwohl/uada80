-- Interfaces.C.Strings for Z80
-- C string pointer handling
--
-- Provides operations on null-terminated C strings

with Interfaces.C;
with System;

package Interfaces.C.Strings is
   pragma Preelaborate;

   -- chars_ptr is reexported from Interfaces.C
   type chars_ptr is new System.Address;

   Null_Ptr : constant chars_ptr := chars_ptr (System.Null_Address);

   -- Exception for null pointer operations
   Dereference_Error : exception;

   -- Create a chars_ptr from an Ada string
   function New_String (Str : String) return chars_ptr;

   -- Create a chars_ptr from a C char array
   function New_Char_Array (Chars : Interfaces.C.char_array) return chars_ptr;

   -- Free memory associated with chars_ptr
   procedure Free (Item : in Out chars_ptr);

   -- Get string value from chars_ptr
   function Value (Item : chars_ptr) return Interfaces.C.char_array;
   function Value (Item : chars_ptr; Length : Interfaces.C.size_t) return Interfaces.C.char_array;

   -- Get length of C string (not including nul)
   function Strlen (Item : chars_ptr) return Interfaces.C.size_t;

   -- Update operations
   procedure Update
     (Item   : chars_ptr;
      Offset : Interfaces.C.size_t;
      Chars  : Interfaces.C.char_array;
      Check  : Boolean := True);

   procedure Update
     (Item   : chars_ptr;
      Offset : Interfaces.C.size_t;
      Str    : String;
      Check  : Boolean := True);

end Interfaces.C.Strings;
