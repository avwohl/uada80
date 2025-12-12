-- GNAT.Encode_UTF8_String for Z80
-- Wide_String to UTF-8 encoding

package GNAT.Encode_UTF8_String is
   pragma Preelaborate;

   function Encode_Wide_String (S : Wide_String) return String;
   --  Encode Wide_String to UTF-8

   function Encode_Wide_Wide_String (S : Wide_Wide_String) return String;
   --  Encode Wide_Wide_String to UTF-8

end GNAT.Encode_UTF8_String;
