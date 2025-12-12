-- Interfaces.C for Z80
-- Provides types compatible with C language

with Interfaces;
with System;

package Interfaces.C is
   pragma Preelaborate;

   -- C character types
   type char is new Character;
   nul : constant char := char'Val (0);

   type signed_char   is new Interfaces.Integer_8;
   type unsigned_char is new Interfaces.Unsigned_8;

   type plain_char is new char;

   -- C integer types (Z80 sizes)
   type int   is new Interfaces.Integer_16;  -- 16-bit int on Z80
   type short is new Interfaces.Integer_16;
   type long  is new Interfaces.Integer_32;

   type unsigned       is new Interfaces.Unsigned_16;
   type unsigned_short is new Interfaces.Unsigned_16;
   type unsigned_long  is new Interfaces.Unsigned_32;

   -- Size types
   type size_t    is new Interfaces.Unsigned_16;
   type ptrdiff_t is new Interfaces.Integer_16;

   -- Pointer types
   type chars_ptr is private;

   Null_Ptr : constant chars_ptr;

   -- Convert between Ada String and C char array
   type char_array is array (size_t range <>) of aliased char;

   function To_C (Item : String; Append_Nul : Boolean := True) return char_array;
   function To_Ada (Item : char_array; Trim_Nul : Boolean := True) return String;

   function To_C (Item : String; Target : out char_array; Count : out size_t; Append_Nul : Boolean := True);

   -- Convert between chars_ptr and String
   function Value (Item : chars_ptr) return String;
   function Value (Item : chars_ptr; Length : size_t) return String;

   function Strlen (Item : chars_ptr) return size_t;

   -- Memory allocation (would need heap support)
   function New_Char_Array (Chars : char_array) return chars_ptr;
   function New_String (Str : String) return chars_ptr;
   procedure Free (Item : in Out chars_ptr);

   -- C wide character types (limited support on Z80)
   type wchar_t is new Interfaces.Unsigned_16;

   -- Floating point (matches z88dk)
   type C_float  is digits 6;
   type double   is digits 6;
   type long_double is digits 6;

private

   type chars_ptr is new System.Address;
   Null_Ptr : constant chars_ptr := chars_ptr (System.Null_Address);

end Interfaces.C;
