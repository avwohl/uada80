-- System.Val_Util for Z80
-- Common utilities for Value attribute implementations

package System.Val_Util is
   pragma Pure;

   -- Skip leading blanks
   procedure Skip_Blanks
     (Str : String;
      Ptr : in Out Integer;
      Max : Integer);
   -- Advances Ptr past any leading blanks

   -- Skip trailing blanks and check nothing else remains
   procedure Check_Trailing_Blanks
     (Str : String;
      Ptr : Integer);
   -- Raises Constraint_Error if non-blank characters after Ptr

   -- Skip leading sign, return True if negative
   procedure Scan_Sign
     (Str      : String;
      Ptr      : in Out Integer;
      Max      : Integer;
      Negative : out Boolean);

   -- Scan optional exponent (E+nn or E-nn)
   procedure Scan_Exponent
     (Str : String;
      Ptr : in Out Integer;
      Max : Integer;
      Exp : out Integer);

   -- Convert hex digit character to value
   function Hex_Digit_Value (C : Character) return Natural;
   -- Returns 0-15, raises Constraint_Error for invalid

   -- Check if character is valid digit for given base
   function Valid_Digit (C : Character; Base : Natural) return Boolean;

end System.Val_Util;
