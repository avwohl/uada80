-- GNAT.Char_Utils body for Z80
-- Character utilities implementation

package body GNAT.Char_Utils is

   ----------------
   -- Is_Control --
   ----------------

   function Is_Control (C : Character) return Boolean is
      Code : constant Natural := Character'Pos (C);
   begin
      return Code < 32 or Code = 127;
   end Is_Control;

   ------------------
   -- Is_Printable --
   ------------------

   function Is_Printable (C : Character) return Boolean is
      Code : constant Natural := Character'Pos (C);
   begin
      return Code >= 32 and Code < 127;
   end Is_Printable;

   ----------------
   -- Is_Graphic --
   ----------------

   function Is_Graphic (C : Character) return Boolean is
      Code : constant Natural := Character'Pos (C);
   begin
      return Code > 32 and Code < 127;
   end Is_Graphic;

   ---------------------
   -- Is_Alphanumeric --
   ---------------------

   function Is_Alphanumeric (C : Character) return Boolean is
   begin
      return C in 'A' .. 'Z' or C in 'a' .. 'z' or C in '0' .. '9';
   end Is_Alphanumeric;

   --------------
   -- Is_Vowel --
   --------------

   function Is_Vowel (C : Character) return Boolean is
   begin
      return C in 'A' | 'E' | 'I' | 'O' | 'U' | 'a' | 'e' | 'i' | 'o' | 'u';
   end Is_Vowel;

   ------------------
   -- Is_Consonant --
   ------------------

   function Is_Consonant (C : Character) return Boolean is
   begin
      return (C in 'A' .. 'Z' or C in 'a' .. 'z') and then not Is_Vowel (C);
   end Is_Consonant;

   ------------------
   -- Is_Hex_Digit --
   ------------------

   function Is_Hex_Digit (C : Character) return Boolean is
   begin
      return C in '0' .. '9' or C in 'A' .. 'F' or C in 'a' .. 'f';
   end Is_Hex_Digit;

   --------------------
   -- Is_Octal_Digit --
   --------------------

   function Is_Octal_Digit (C : Character) return Boolean is
   begin
      return C in '0' .. '7';
   end Is_Octal_Digit;

   ---------------------
   -- Is_Binary_Digit --
   ---------------------

   function Is_Binary_Digit (C : Character) return Boolean is
   begin
      return C in '0' .. '1';
   end Is_Binary_Digit;

   ---------------
   -- Hex_Value --
   ---------------

   function Hex_Value (C : Character) return Natural is
   begin
      if C in '0' .. '9' then
         return Character'Pos (C) - Character'Pos ('0');
      elsif C in 'A' .. 'F' then
         return Character'Pos (C) - Character'Pos ('A') + 10;
      elsif C in 'a' .. 'f' then
         return Character'Pos (C) - Character'Pos ('a') + 10;
      else
         return 0;
      end if;
   end Hex_Value;

   ------------
   -- To_Hex --
   ------------

   function To_Hex (Value : Natural) return Character is
      Hex_Chars : constant String := "0123456789ABCDEF";
   begin
      if Value < 16 then
         return Hex_Chars (Value + 1);
      else
         return '?';
      end if;
   end To_Hex;

   ---------------
   -- Char_Code --
   ---------------

   function Char_Code (C : Character) return Natural is
   begin
      return Character'Pos (C);
   end Char_Code;

   ---------------
   -- Code_Char --
   ---------------

   function Code_Char (Code : Natural) return Character is
   begin
      if Code < 256 then
         return Character'Val (Code);
      else
         return Character'Val (0);
      end if;
   end Code_Char;

end GNAT.Char_Utils;
