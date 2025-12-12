-- Ada.Wide_Wide_Characters.Handling body for Z80
-- Wide wide character classification (Latin-1 subset only)

package body Ada.Wide_Wide_Characters.Handling is

   ----------------
   -- Is_Control --
   ----------------

   function Is_Control (Item : Wide_Wide_Character) return Boolean is
      Pos : constant Natural := Wide_Wide_Character'Pos (Item);
   begin
      return Pos <= 31 or (Pos >= 127 and Pos <= 159);
   end Is_Control;

   ---------------
   -- Is_Letter --
   ---------------

   function Is_Letter (Item : Wide_Wide_Character) return Boolean is
   begin
      return Is_Lower (Item) or Is_Upper (Item);
   end Is_Letter;

   --------------
   -- Is_Lower --
   --------------

   function Is_Lower (Item : Wide_Wide_Character) return Boolean is
      Pos : constant Natural := Wide_Wide_Character'Pos (Item);
   begin
      return (Pos >= 97 and Pos <= 122) or     -- a-z
             (Pos >= 223 and Pos <= 246) or    -- Latin-1 lowercase
             (Pos >= 248 and Pos <= 255);      -- Latin-1 lowercase
   end Is_Lower;

   --------------
   -- Is_Upper --
   --------------

   function Is_Upper (Item : Wide_Wide_Character) return Boolean is
      Pos : constant Natural := Wide_Wide_Character'Pos (Item);
   begin
      return (Pos >= 65 and Pos <= 90) or      -- A-Z
             (Pos >= 192 and Pos <= 214) or    -- Latin-1 uppercase
             (Pos >= 216 and Pos <= 222);      -- Latin-1 uppercase
   end Is_Upper;

   --------------
   -- Is_Digit --
   --------------

   function Is_Digit (Item : Wide_Wide_Character) return Boolean is
      Pos : constant Natural := Wide_Wide_Character'Pos (Item);
   begin
      return Pos >= 48 and Pos <= 57;  -- 0-9
   end Is_Digit;

   ----------------------
   -- Is_Decimal_Digit --
   ----------------------

   function Is_Decimal_Digit (Item : Wide_Wide_Character) return Boolean is
   begin
      return Is_Digit (Item);
   end Is_Decimal_Digit;

   --------------------------
   -- Is_Hexadecimal_Digit --
   --------------------------

   function Is_Hexadecimal_Digit (Item : Wide_Wide_Character) return Boolean is
      Pos : constant Natural := Wide_Wide_Character'Pos (Item);
   begin
      return Is_Digit (Item) or
             (Pos >= 65 and Pos <= 70) or   -- A-F
             (Pos >= 97 and Pos <= 102);    -- a-f
   end Is_Hexadecimal_Digit;

   --------------------
   -- Is_Alphanumeric --
   --------------------

   function Is_Alphanumeric (Item : Wide_Wide_Character) return Boolean is
   begin
      return Is_Letter (Item) or Is_Digit (Item);
   end Is_Alphanumeric;

   ----------------
   -- Is_Special --
   ----------------

   function Is_Special (Item : Wide_Wide_Character) return Boolean is
      Pos : constant Natural := Wide_Wide_Character'Pos (Item);
   begin
      return (Pos >= 33 and Pos <= 47) or    -- !"#$%&'()*+,-./
             (Pos >= 58 and Pos <= 64) or    -- :;<=>?@
             (Pos >= 91 and Pos <= 96) or    -- [\]^_`
             (Pos >= 123 and Pos <= 126);    -- {|}~
   end Is_Special;

   -----------------------
   -- Is_Line_Terminator --
   -----------------------

   function Is_Line_Terminator (Item : Wide_Wide_Character) return Boolean is
      Pos : constant Natural := Wide_Wide_Character'Pos (Item);
   begin
      return Pos = 10 or   -- LF
             Pos = 11 or   -- VT
             Pos = 12 or   -- FF
             Pos = 13 or   -- CR
             Pos = 133;    -- NEL
   end Is_Line_Terminator;

   -------------
   -- Is_Mark --
   -------------

   function Is_Mark (Item : Wide_Wide_Character) return Boolean is
   begin
      -- No combining marks in Latin-1
      return False;
   end Is_Mark;

   --------------
   -- Is_Space --
   --------------

   function Is_Space (Item : Wide_Wide_Character) return Boolean is
      Pos : constant Natural := Wide_Wide_Character'Pos (Item);
   begin
      return Pos = 32 or Pos = 160;  -- Space and no-break space
   end Is_Space;

   ----------------
   -- Is_Graphic --
   ----------------

   function Is_Graphic (Item : Wide_Wide_Character) return Boolean is
      Pos : constant Natural := Wide_Wide_Character'Pos (Item);
   begin
      return (Pos >= 32 and Pos <= 126) or (Pos >= 160 and Pos <= 255);
   end Is_Graphic;

   --------------
   -- Is_Basic --
   --------------

   function Is_Basic (Item : Wide_Wide_Character) return Boolean is
      Pos : constant Natural := Wide_Wide_Character'Pos (Item);
   begin
      return (Pos >= 65 and Pos <= 90) or (Pos >= 97 and Pos <= 122);
   end Is_Basic;

   --------------
   -- To_Basic --
   --------------

   function To_Basic (Item : Wide_Wide_Character) return Wide_Wide_Character is
      Pos : constant Natural := Wide_Wide_Character'Pos (Item);
   begin
      case Pos is
         when 192 .. 198 => return 'A';
         when 199        => return 'C';
         when 200 .. 203 => return 'E';
         when 204 .. 207 => return 'I';
         when 208        => return 'D';
         when 209        => return 'N';
         when 210 .. 214 => return 'O';
         when 216        => return 'O';
         when 217 .. 220 => return 'U';
         when 221        => return 'Y';
         when 222        => return 'P';
         when 223        => return 's';
         when 224 .. 230 => return 'a';
         when 231        => return 'c';
         when 232 .. 235 => return 'e';
         when 236 .. 239 => return 'i';
         when 240        => return 'd';
         when 241        => return 'n';
         when 242 .. 246 => return 'o';
         when 248        => return 'o';
         when 249 .. 252 => return 'u';
         when 253 | 255  => return 'y';
         when 254        => return 'p';
         when others     => return Item;
      end case;
   end To_Basic;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (Item : Wide_Wide_Character) return Wide_Wide_Character is
      Pos : constant Natural := Wide_Wide_Character'Pos (Item);
   begin
      if Pos >= 65 and Pos <= 90 then
         return Wide_Wide_Character'Val (Pos + 32);
      elsif Pos >= 192 and Pos <= 214 then
         return Wide_Wide_Character'Val (Pos + 32);
      elsif Pos >= 216 and Pos <= 222 then
         return Wide_Wide_Character'Val (Pos + 32);
      else
         return Item;
      end if;
   end To_Lower;

   --------------
   -- To_Upper --
   --------------

   function To_Upper (Item : Wide_Wide_Character) return Wide_Wide_Character is
      Pos : constant Natural := Wide_Wide_Character'Pos (Item);
   begin
      if Pos >= 97 and Pos <= 122 then
         return Wide_Wide_Character'Val (Pos - 32);
      elsif Pos >= 224 and Pos <= 246 then
         return Wide_Wide_Character'Val (Pos - 32);
      elsif Pos >= 248 and Pos <= 254 then
         return Wide_Wide_Character'Val (Pos - 32);
      else
         return Item;
      end if;
   end To_Upper;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (Item : Wide_Wide_String) return Wide_Wide_String is
      Result : Wide_Wide_String (Item'Range);
   begin
      for I in Item'Range loop
         Result (I) := To_Lower (Item (I));
      end loop;
      return Result;
   end To_Lower;

   --------------
   -- To_Upper --
   --------------

   function To_Upper (Item : Wide_Wide_String) return Wide_Wide_String is
      Result : Wide_Wide_String (Item'Range);
   begin
      for I in Item'Range loop
         Result (I) := To_Upper (Item (I));
      end loop;
      return Result;
   end To_Upper;

end Ada.Wide_Wide_Characters.Handling;
