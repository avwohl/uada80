-- GNAT.Encode_String for Z80
-- String encoding utilities

package GNAT.Encode_String is
   pragma Pure;

   function Encode_Hex (S : String) return String;
   --  Encode string as hexadecimal (e.g., "Hello" -> "48656C6C6F")

   function Encode_Base64 (S : String) return String;
   --  Encode string as Base64

   function Encode_URL (S : String) return String;
   --  URL-encode string (e.g., " " -> "%20")

   function Encode_Escape (S : String) return String;
   --  Encode special characters as C-style escapes

end GNAT.Encode_String;
