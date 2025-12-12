-- Ada.Locales.Generic_Enumeration body for Z80
-- Locale-aware enumeration I/O implementation

package body Ada.Locales.Generic_Enumeration is

   ----------------
   -- Wide_Image --
   ----------------

   function Wide_Image (Item : Enum) return Wide_String is
      S : constant String := Enum'Image (Item);
      Result : Wide_String (S'Range);
   begin
      for I in S'Range loop
         Result (I) := Wide_Character'Val (Character'Pos (S (I)));
      end loop;
      return Result;
   end Wide_Image;

   ---------------------
   -- Wide_Wide_Image --
   ---------------------

   function Wide_Wide_Image (Item : Enum) return Wide_Wide_String is
      S : constant String := Enum'Image (Item);
      Result : Wide_Wide_String (S'Range);
   begin
      for I in S'Range loop
         Result (I) := Wide_Wide_Character'Val (Character'Pos (S (I)));
      end loop;
      return Result;
   end Wide_Wide_Image;

   -----------
   -- Value --
   -----------

   function Value (Item : String) return Enum is
   begin
      return Enum'Value (Item);
   end Value;

   ----------------
   -- Wide_Value --
   ----------------

   function Wide_Value (Item : Wide_String) return Enum is
      S : String (Item'Range);
   begin
      for I in Item'Range loop
         if Wide_Character'Pos (Item (I)) < 256 then
            S (I) := Character'Val (Wide_Character'Pos (Item (I)));
         else
            S (I) := '?';
         end if;
      end loop;
      return Enum'Value (S);
   end Wide_Value;

   ---------------------
   -- Wide_Wide_Value --
   ---------------------

   function Wide_Wide_Value (Item : Wide_Wide_String) return Enum is
      S : String (Item'Range);
   begin
      for I in Item'Range loop
         if Wide_Wide_Character'Pos (Item (I)) < 256 then
            S (I) := Character'Val (Wide_Wide_Character'Pos (Item (I)));
         else
            S (I) := '?';
         end if;
      end loop;
      return Enum'Value (S);
   end Wide_Wide_Value;

end Ada.Locales.Generic_Enumeration;
