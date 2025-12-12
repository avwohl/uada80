-- System.UTF_32 for Z80
-- UTF-32 character handling support

package System.UTF_32 is
   pragma Pure;

   -- UTF-32 code point type
   type UTF_32_Code is mod 2 ** 32;

   -- Unicode code point boundaries
   Max_UTF_32 : constant := 16#10FFFF#;

   -- Surrogate pair range (invalid in UTF-32)
   Surrogate_First : constant := 16#D800#;
   Surrogate_Last  : constant := 16#DFFF#;

   -- Check if code point is valid
   function Is_Valid (Code : UTF_32_Code) return Boolean;

   -- Check character categories
   function Is_Letter (Code : UTF_32_Code) return Boolean;
   function Is_Digit (Code : UTF_32_Code) return Boolean;
   function Is_Alphanumeric (Code : UTF_32_Code) return Boolean;
   function Is_Space (Code : UTF_32_Code) return Boolean;
   function Is_Control (Code : UTF_32_Code) return Boolean;
   function Is_Graphic (Code : UTF_32_Code) return Boolean;

   -- Case conversion (Basic Latin only on Z80)
   function To_Upper (Code : UTF_32_Code) return UTF_32_Code;
   function To_Lower (Code : UTF_32_Code) return UTF_32_Code;

   -- Conversion to/from Wide_Wide_Character
   function To_Wide_Wide_Char (Code : UTF_32_Code) return Wide_Wide_Character;
   function From_Wide_Wide_Char (C : Wide_Wide_Character) return UTF_32_Code;

end System.UTF_32;
