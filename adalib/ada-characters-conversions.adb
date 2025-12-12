-- Ada.Characters.Conversions body for Z80
-- Character conversion implementations

package body Ada.Characters.Conversions is

   -----------------------
   -- To_Wide_Character --
   -----------------------

   function To_Wide_Character (Item : Character) return Wide_Character is
   begin
      return Wide_Character'Val (Character'Pos (Item));
   end To_Wide_Character;

   ------------------
   -- To_Character --
   ------------------

   function To_Character
     (Item       : Wide_Character;
      Substitute : Character := ' ') return Character
   is
      Code : constant Natural := Wide_Character'Pos (Item);
   begin
      if Code <= 255 then
         return Character'Val (Code);
      else
         return Substitute;
      end if;
   end To_Character;

   ------------------
   -- Is_Character --
   ------------------

   function Is_Character (Item : Wide_Character) return Boolean is
   begin
      return Wide_Character'Pos (Item) <= 255;
   end Is_Character;

   --------------------
   -- To_Wide_String --
   --------------------

   function To_Wide_String (Item : String) return Wide_String is
      Result : Wide_String (Item'Range);
   begin
      for I in Item'Range loop
         Result (I) := To_Wide_Character (Item (I));
      end loop;
      return Result;
   end To_Wide_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Item       : Wide_String;
      Substitute : Character := ' ') return String
   is
      Result : String (Item'Range);
   begin
      for I in Item'Range loop
         Result (I) := To_Character (Item (I), Substitute);
      end loop;
      return Result;
   end To_String;

   ----------------------------
   -- To_Wide_Wide_Character --
   ----------------------------

   function To_Wide_Wide_Character (Item : Character) return Wide_Wide_Character is
   begin
      return Wide_Wide_Character'Val (Character'Pos (Item));
   end To_Wide_Wide_Character;

   ------------------
   -- To_Character --
   ------------------

   function To_Character
     (Item       : Wide_Wide_Character;
      Substitute : Character := ' ') return Character
   is
      Code : constant Natural := Wide_Wide_Character'Pos (Item);
   begin
      if Code <= 255 then
         return Character'Val (Code);
      else
         return Substitute;
      end if;
   end To_Character;

   ------------------
   -- Is_Character --
   ------------------

   function Is_Character (Item : Wide_Wide_Character) return Boolean is
   begin
      return Wide_Wide_Character'Pos (Item) <= 255;
   end Is_Character;

   -------------------------
   -- To_Wide_Wide_String --
   -------------------------

   function To_Wide_Wide_String (Item : String) return Wide_Wide_String is
      Result : Wide_Wide_String (Item'Range);
   begin
      for I in Item'Range loop
         Result (I) := To_Wide_Wide_Character (Item (I));
      end loop;
      return Result;
   end To_Wide_Wide_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Item       : Wide_Wide_String;
      Substitute : Character := ' ') return String
   is
      Result : String (Item'Range);
   begin
      for I in Item'Range loop
         Result (I) := To_Character (Item (I), Substitute);
      end loop;
      return Result;
   end To_String;

   ----------------------------
   -- To_Wide_Wide_Character --
   ----------------------------

   function To_Wide_Wide_Character (Item : Wide_Character) return Wide_Wide_Character is
   begin
      return Wide_Wide_Character'Val (Wide_Character'Pos (Item));
   end To_Wide_Wide_Character;

   -----------------------
   -- To_Wide_Character --
   -----------------------

   function To_Wide_Character
     (Item       : Wide_Wide_Character;
      Substitute : Wide_Character := ' ') return Wide_Character
   is
      Code : constant Natural := Wide_Wide_Character'Pos (Item);
   begin
      if Code <= 65535 then
         return Wide_Character'Val (Code);
      else
         return Substitute;
      end if;
   end To_Wide_Character;

   -----------------------
   -- Is_Wide_Character --
   -----------------------

   function Is_Wide_Character (Item : Wide_Wide_Character) return Boolean is
   begin
      return Wide_Wide_Character'Pos (Item) <= 65535;
   end Is_Wide_Character;

   -------------------------
   -- To_Wide_Wide_String --
   -------------------------

   function To_Wide_Wide_String (Item : Wide_String) return Wide_Wide_String is
      Result : Wide_Wide_String (Item'Range);
   begin
      for I in Item'Range loop
         Result (I) := To_Wide_Wide_Character (Item (I));
      end loop;
      return Result;
   end To_Wide_Wide_String;

   --------------------
   -- To_Wide_String --
   --------------------

   function To_Wide_String
     (Item       : Wide_Wide_String;
      Substitute : Wide_Character := ' ') return Wide_String
   is
      Result : Wide_String (Item'Range);
   begin
      for I in Item'Range loop
         Result (I) := To_Wide_Character (Item (I), Substitute);
      end loop;
      return Result;
   end To_Wide_String;

end Ada.Characters.Conversions;
