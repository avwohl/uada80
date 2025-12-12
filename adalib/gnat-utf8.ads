-- GNAT.UTF8 for Z80
-- UTF-8 encoding/decoding utilities

package GNAT.UTF8 is
   pragma Pure;

   type Code_Point is mod 2**21;
   --  Unicode code point (21 bits)

   function Is_Valid_UTF8 (S : String) return Boolean;
   --  Check if string is valid UTF-8

   function Length (S : String) return Natural;
   --  Return number of UTF-8 characters (not bytes)

   function Get_Char
     (S     : String;
      Index : Positive) return Code_Point;
   --  Get UTF-8 character at byte position

   function Get_Char_Length (C : Character) return Natural;
   --  Return byte length of UTF-8 sequence starting with C

   function Encode (CP : Code_Point) return String;
   --  Encode code point to UTF-8 string

   function Decode
     (S     : String;
      Index : Positive;
      CP    : out Code_Point) return Natural;
   --  Decode UTF-8 at Index, return bytes consumed

   function To_Lower (S : String) return String;
   --  Convert ASCII characters to lower case

   function To_Upper (S : String) return String;
   --  Convert ASCII characters to upper case

end GNAT.UTF8;
