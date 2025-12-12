-- System.UTF_32 body for Z80
-- UTF-32 character handling implementation

package body System.UTF_32 is

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Code : UTF_32_Code) return Boolean is
   begin
      return Code <= Max_UTF_32 and
             (Code < Surrogate_First or Code > Surrogate_Last);
   end Is_Valid;

   ---------------
   -- Is_Letter --
   ---------------

   function Is_Letter (Code : UTF_32_Code) return Boolean is
   begin
      -- Basic Latin and Latin-1 Supplement letters
      return (Code >= 16#41# and Code <= 16#5A#) or      -- A-Z
             (Code >= 16#61# and Code <= 16#7A#) or      -- a-z
             (Code >= 16#C0# and Code <= 16#D6#) or      -- Latin-1 uppercase
             (Code >= 16#D8# and Code <= 16#F6#) or      -- Latin-1
             (Code >= 16#F8# and Code <= 16#FF#);        -- Latin-1 lowercase
   end Is_Letter;

   --------------
   -- Is_Digit --
   --------------

   function Is_Digit (Code : UTF_32_Code) return Boolean is
   begin
      return Code >= 16#30# and Code <= 16#39#;  -- 0-9
   end Is_Digit;

   ---------------------
   -- Is_Alphanumeric --
   ---------------------

   function Is_Alphanumeric (Code : UTF_32_Code) return Boolean is
   begin
      return Is_Letter (Code) or Is_Digit (Code);
   end Is_Alphanumeric;

   --------------
   -- Is_Space --
   --------------

   function Is_Space (Code : UTF_32_Code) return Boolean is
   begin
      return Code = 16#20# or                            -- Space
             (Code >= 16#09# and Code <= 16#0D#) or      -- HT, LF, VT, FF, CR
             Code = 16#A0#;                              -- No-break space
   end Is_Space;

   ----------------
   -- Is_Control --
   ----------------

   function Is_Control (Code : UTF_32_Code) return Boolean is
   begin
      return Code <= 16#1F# or
             (Code >= 16#7F# and Code <= 16#9F#);
   end Is_Control;

   ----------------
   -- Is_Graphic --
   ----------------

   function Is_Graphic (Code : UTF_32_Code) return Boolean is
   begin
      return not Is_Control (Code) and Code /= 16#20#;
   end Is_Graphic;

   --------------
   -- To_Upper --
   --------------

   function To_Upper (Code : UTF_32_Code) return UTF_32_Code is
   begin
      -- ASCII lowercase to uppercase
      if Code >= 16#61# and Code <= 16#7A# then
         return Code - 32;
      end if;

      -- Latin-1 lowercase to uppercase
      if Code >= 16#E0# and Code <= 16#F6# then
         return Code - 32;
      end if;
      if Code >= 16#F8# and Code <= 16#FE# then
         return Code - 32;
      end if;

      return Code;
   end To_Upper;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (Code : UTF_32_Code) return UTF_32_Code is
   begin
      -- ASCII uppercase to lowercase
      if Code >= 16#41# and Code <= 16#5A# then
         return Code + 32;
      end if;

      -- Latin-1 uppercase to lowercase
      if Code >= 16#C0# and Code <= 16#D6# then
         return Code + 32;
      end if;
      if Code >= 16#D8# and Code <= 16#DE# then
         return Code + 32;
      end if;

      return Code;
   end To_Lower;

   -----------------------
   -- To_Wide_Wide_Char --
   -----------------------

   function To_Wide_Wide_Char (Code : UTF_32_Code) return Wide_Wide_Character is
   begin
      return Wide_Wide_Character'Val (Natural (Code));
   end To_Wide_Wide_Char;

   -------------------------
   -- From_Wide_Wide_Char --
   -------------------------

   function From_Wide_Wide_Char (C : Wide_Wide_Character) return UTF_32_Code is
   begin
      return UTF_32_Code (Wide_Wide_Character'Pos (C));
   end From_Wide_Wide_Char;

end System.UTF_32;
