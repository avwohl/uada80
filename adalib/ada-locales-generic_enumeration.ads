-- Ada.Locales.Generic_Enumeration for Z80
-- Locale-aware enumeration I/O

with Ada.Locales;

generic
   type Enum is (<>);
package Ada.Locales.Generic_Enumeration is

   function Wide_Image (Item : Enum) return Wide_String;
   function Wide_Wide_Image (Item : Enum) return Wide_Wide_String;

   function Value (Item : String) return Enum;
   function Wide_Value (Item : Wide_String) return Enum;
   function Wide_Wide_Value (Item : Wide_Wide_String) return Enum;

end Ada.Locales.Generic_Enumeration;
