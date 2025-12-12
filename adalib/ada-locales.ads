-- Ada.Locales for Z80
-- Locale information
--
-- Note: Z80/CP/M systems typically only support ASCII.
-- This implementation provides a minimal C locale.

package Ada.Locales is
   pragma Preelaborate;

   type Language_Code is new String (1 .. 3);
   type Country_Code is new String (1 .. 2);

   -- Default locale is "C" (POSIX)
   Language_Unknown : constant Language_Code := "   ";
   Country_Unknown  : constant Country_Code := "  ";

   function Language return Language_Code;
   -- Returns the language code for the current locale

   function Country return Country_Code;
   -- Returns the country code for the current locale

end Ada.Locales;
