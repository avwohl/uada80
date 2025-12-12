-- System.Val_Util body for Z80
-- Common utilities for Value attribute implementations

package body System.Val_Util is

   ------------------
   -- Skip_Blanks --
   ------------------

   procedure Skip_Blanks
     (Str : String;
      Ptr : in Out Integer;
      Max : Integer)
   is
   begin
      while Ptr <= Max and then Str (Ptr) = ' ' loop
         Ptr := Ptr + 1;
      end loop;
   end Skip_Blanks;

   ----------------------------
   -- Check_Trailing_Blanks --
   ----------------------------

   procedure Check_Trailing_Blanks
     (Str : String;
      Ptr : Integer)
   is
      P : Integer := Ptr;
   begin
      while P <= Str'Last loop
         if Str (P) /= ' ' then
            raise Constraint_Error;
         end if;
         P := P + 1;
      end loop;
   end Check_Trailing_Blanks;

   ---------------
   -- Scan_Sign --
   ---------------

   procedure Scan_Sign
     (Str      : String;
      Ptr      : in Out Integer;
      Max      : Integer;
      Negative : out Boolean)
   is
   begin
      Negative := False;

      if Ptr <= Max then
         if Str (Ptr) = '-' then
            Negative := True;
            Ptr := Ptr + 1;
         elsif Str (Ptr) = '+' then
            Ptr := Ptr + 1;
         end if;
      end if;
   end Scan_Sign;

   -------------------
   -- Scan_Exponent --
   -------------------

   procedure Scan_Exponent
     (Str : String;
      Ptr : in Out Integer;
      Max : Integer;
      Exp : out Integer)
   is
      Neg : Boolean := False;
   begin
      Exp := 0;

      if Ptr > Max then
         return;
      end if;

      -- Check for E or e
      if Str (Ptr) /= 'E' and Str (Ptr) /= 'e' then
         return;
      end if;

      Ptr := Ptr + 1;

      if Ptr > Max then
         Ptr := Ptr - 1;  -- Revert if no exponent follows
         return;
      end if;

      -- Check for sign
      if Str (Ptr) = '-' then
         Neg := True;
         Ptr := Ptr + 1;
      elsif Str (Ptr) = '+' then
         Ptr := Ptr + 1;
      end if;

      -- Parse exponent digits
      if Ptr > Max or else Str (Ptr) not in '0' .. '9' then
         raise Constraint_Error;
      end if;

      while Ptr <= Max and then Str (Ptr) in '0' .. '9' loop
         Exp := Exp * 10 + (Character'Pos (Str (Ptr)) - Character'Pos ('0'));
         Ptr := Ptr + 1;
      end loop;

      if Neg then
         Exp := -Exp;
      end if;
   end Scan_Exponent;

   ---------------------
   -- Hex_Digit_Value --
   ---------------------

   function Hex_Digit_Value (C : Character) return Natural is
   begin
      case C is
         when '0' .. '9' =>
            return Character'Pos (C) - Character'Pos ('0');
         when 'A' .. 'F' =>
            return Character'Pos (C) - Character'Pos ('A') + 10;
         when 'a' .. 'f' =>
            return Character'Pos (C) - Character'Pos ('a') + 10;
         when others =>
            raise Constraint_Error;
      end case;
   end Hex_Digit_Value;

   -----------------
   -- Valid_Digit --
   -----------------

   function Valid_Digit (C : Character; Base : Natural) return Boolean is
      Val : Natural;
   begin
      case C is
         when '0' .. '9' =>
            Val := Character'Pos (C) - Character'Pos ('0');
         when 'A' .. 'Z' =>
            Val := Character'Pos (C) - Character'Pos ('A') + 10;
         when 'a' .. 'z' =>
            Val := Character'Pos (C) - Character'Pos ('a') + 10;
         when others =>
            return False;
      end case;

      return Val < Base;
   end Valid_Digit;

end System.Val_Util;
