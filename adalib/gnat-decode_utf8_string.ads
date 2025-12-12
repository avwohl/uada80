-- GNAT.Decode_UTF8_String for Z80
-- UTF-8 to Wide_String decoding

package GNAT.Decode_UTF8_String is
   pragma Preelaborate;

   function Decode_Wide_String (S : String) return Wide_String;
   --  Decode UTF-8 string to Wide_String

   function Decode_Wide_Wide_String (S : String) return Wide_Wide_String;
   --  Decode UTF-8 string to Wide_Wide_String

end GNAT.Decode_UTF8_String;
