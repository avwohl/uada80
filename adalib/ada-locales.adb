-- Ada.Locales body for Z80
-- Locale information implementation

package body Ada.Locales is

   --------------
   -- Language --
   --------------

   function Language return Language_Code is
   begin
      -- Return "eng" for English (ISO 639-3)
      -- On Z80/CP/M, locale is not configurable
      return "eng";
   end Language;

   -------------
   -- Country --
   -------------

   function Country return Country_Code is
   begin
      -- Return "US" as default country
      return "US";
   end Country;

end Ada.Locales;
