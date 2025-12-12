-- Interfaces.COBOL for Z80
-- Types for interfacing with COBOL
--
-- Provides Ada equivalents of COBOL intrinsic types

with Interfaces;

package Interfaces.COBOL is
   pragma Preelaborate;

   -- COBOL Character types
   type COBOL_Character is new Character;

   -- Alphanumeric types
   type Alphanumeric is array (Positive range <>) of COBOL_Character;

   -- Numeric types (zoned decimal)
   -- COBOL's numeric types with USAGE DISPLAY
   type Numeric is array (Positive range <>) of COBOL_Character;

   -- Binary types (COBOL COMP/BINARY)
   type Binary is new Interfaces.Integer_16;
   type Long_Binary is new Interfaces.Integer_32;

   -- Unsigned binary
   type Unsigned_Binary is new Interfaces.Unsigned_16;
   type Unsigned_Long_Binary is new Interfaces.Unsigned_32;

   -- Decimal types (limited support on Z80)
   Max_Digits_Binary      : constant := 9;
   Max_Digits_Long_Binary : constant := 18;

   type Byte is new Interfaces.Unsigned_8;
   type Byte_Array is array (Positive range <>) of Byte;

   -- Packed decimal (COMP-3)
   type Packed_Decimal is array (Positive range <>) of Byte;

   -- Display picture types
   subtype Display_Format is String;

   -- Conversion error exception
   Conversion_Error : exception;

   -- Conversion functions: Ada to COBOL
   function To_COBOL (Item : Character) return COBOL_Character;
   function To_COBOL (Item : String) return Alphanumeric;

   -- Conversion functions: COBOL to Ada
   function To_Ada (Item : COBOL_Character) return Character;
   function To_Ada (Item : Alphanumeric) return String;

   -- Procedure versions with explicit target
   procedure To_COBOL
     (Item   : String;
      Target : out Alphanumeric;
      Last   : out Natural);

   procedure To_Ada
     (Item   : Alphanumeric;
      Target : out String;
      Last   : out Natural);

   -- Numeric conversions
   function To_Display
     (Item   : Long_Binary;
      Format : Display_Format) return Numeric;

   function To_Binary (Item : Numeric) return Long_Binary;

   function To_Long_Binary (Item : Numeric) return Long_Binary renames To_Binary;

   -- Packed decimal conversions
   function To_Decimal
     (Item   : Packed_Decimal;
      Format : Display_Format) return Numeric;

   function To_Packed
     (Item   : Numeric;
      Length : Natural) return Packed_Decimal;

   -- Valid format checking
   function Valid (Item : Numeric) return Boolean;

   function Valid
     (Item   : Packed_Decimal;
      Format : Display_Format) return Boolean;

end Interfaces.COBOL;
