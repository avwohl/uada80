-- GNAT.Decode_String for Z80
-- String decoding utilities

package GNAT.Decode_String is
   pragma Pure;

   function Decode_Hex (S : String) return String;
   --  Decode hexadecimal string (e.g., "48656C6C6F" -> "Hello")

   function Decode_Base64 (S : String) return String;
   --  Decode Base64 string

   function Decode_URL (S : String) return String;
   --  Decode URL-encoded string (e.g., "%20" -> " ")

   function Decode_Escape (S : String) return String;
   --  Decode C-style escape sequences

end GNAT.Decode_String;
