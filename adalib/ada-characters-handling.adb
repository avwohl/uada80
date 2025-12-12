-- Ada.Characters.Handling body for Z80
-- Character classification and conversion implementations

package body Ada.Characters.Handling is

   -- Character code ranges
   NUL : constant := 0;
   DEL : constant := 127;

   -- Control character check (0-31 and 127)
   function Is_Control (Item : Character) return Boolean is
      Code : constant Integer := Character'Pos (Item);
   begin
      return Code < 32 or Code = DEL;
   end Is_Control;

   -- Graphic character check (32-126)
   function Is_Graphic (Item : Character) return Boolean is
      Code : constant Integer := Character'Pos (Item);
   begin
      return Code >= 32 and Code < DEL;
   end Is_Graphic;

   -- Letter check (A-Z, a-z)
   function Is_Letter (Item : Character) return Boolean is
   begin
      return Is_Upper (Item) or Is_Lower (Item);
   end Is_Letter;

   -- Lowercase check (a-z)
   function Is_Lower (Item : Character) return Boolean is
   begin
      return Item >= 'a' and Item <= 'z';
   end Is_Lower;

   -- Uppercase check (A-Z)
   function Is_Upper (Item : Character) return Boolean is
   begin
      return Item >= 'A' and Item <= 'Z';
   end Is_Upper;

   -- Basic character (same as letter for ASCII subset)
   function Is_Basic (Item : Character) return Boolean is
   begin
      return Is_Letter (Item);
   end Is_Basic;

   -- Digit check (0-9)
   function Is_Digit (Item : Character) return Boolean is
   begin
      return Item >= '0' and Item <= '9';
   end Is_Digit;

   -- Hexadecimal digit check (0-9, A-F, a-f)
   function Is_Hexadecimal_Digit (Item : Character) return Boolean is
   begin
      return Is_Digit (Item)
        or (Item >= 'A' and Item <= 'F')
        or (Item >= 'a' and Item <= 'f');
   end Is_Hexadecimal_Digit;

   -- Alphanumeric check
   function Is_Alphanumeric (Item : Character) return Boolean is
   begin
      return Is_Letter (Item) or Is_Digit (Item);
   end Is_Alphanumeric;

   -- Special character check (graphic non-alphanumeric)
   function Is_Special (Item : Character) return Boolean is
   begin
      return Is_Graphic (Item) and not Is_Alphanumeric (Item);
   end Is_Special;

   -- Convert to lowercase
   function To_Lower (Item : Character) return Character is
   begin
      if Is_Upper (Item) then
         return Character'Val (Character'Pos (Item) + 32);
      else
         return Item;
      end if;
   end To_Lower;

   -- Convert to uppercase
   function To_Upper (Item : Character) return Character is
   begin
      if Is_Lower (Item) then
         return Character'Val (Character'Pos (Item) - 32);
      else
         return Item;
      end if;
   end To_Upper;

   -- Convert to basic form (for ASCII, same as identity)
   function To_Basic (Item : Character) return Character is
   begin
      return Item;
   end To_Basic;

   -- Check if string contains only graphics and spaces
   function Is_String (Item : String) return Boolean is
   begin
      for I in Item'Range loop
         if not Is_Graphic (Item (I)) then
            return False;
         end if;
      end loop;
      return True;
   end Is_String;

   -- String lowercase conversion
   function To_Lower (Item : String) return String is
      Result : String (Item'Range);
   begin
      for I in Item'Range loop
         Result (I) := To_Lower (Item (I));
      end loop;
      return Result;
   end To_Lower;

   -- String uppercase conversion
   function To_Upper (Item : String) return String is
      Result : String (Item'Range);
   begin
      for I in Item'Range loop
         Result (I) := To_Upper (Item (I));
      end loop;
      return Result;
   end To_Upper;

   -- String basic conversion
   function To_Basic (Item : String) return String is
      Result : String (Item'Range);
   begin
      for I in Item'Range loop
         Result (I) := To_Basic (Item (I));
      end loop;
      return Result;
   end To_Basic;

   -- ISO 646 check (7-bit ASCII)
   function Is_ISO_646 (Item : Character) return Boolean is
   begin
      return Character'Pos (Item) <= 127;
   end Is_ISO_646;

   function Is_ISO_646 (Item : String) return Boolean is
   begin
      for I in Item'Range loop
         if not Is_ISO_646 (Item (I)) then
            return False;
         end if;
      end loop;
      return True;
   end Is_ISO_646;

   -- Convert to ISO 646
   function To_ISO_646 (Item : Character; Substitute : ISO_646 := ' ') return ISO_646 is
   begin
      if Is_ISO_646 (Item) then
         return Item;
      else
         return Substitute;
      end if;
   end To_ISO_646;

   function To_ISO_646 (Item : String; Substitute : ISO_646 := ' ') return String is
      Result : String (Item'Range);
   begin
      for I in Item'Range loop
         Result (I) := To_ISO_646 (Item (I), Substitute);
      end loop;
      return Result;
   end To_ISO_646;

end Ada.Characters.Handling;
