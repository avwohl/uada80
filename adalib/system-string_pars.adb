-- System.String_Pars body for Z80
-- String parsing utilities

package body System.String_Pars is

   -----------------
   -- Skip_Blanks --
   -----------------

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

   ----------------------
   -- Skip_To_Nonblank --
   ----------------------

   procedure Skip_To_Nonblank
     (Str : String;
      Ptr : in Out Integer;
      Max : Integer)
   is
   begin
      Skip_Blanks (Str, Ptr, Max);
   end Skip_To_Nonblank;

   ------------
   -- At_End --
   ------------

   function At_End (Str : String; Ptr : Integer; Max : Integer) return Boolean is
      P : Integer := Ptr;
   begin
      while P <= Max loop
         if Str (P) /= ' ' then
            return False;
         end if;
         P := P + 1;
      end loop;
      return True;
   end At_End;

   --------------
   -- Is_Digit --
   --------------

   function Is_Digit (C : Character) return Boolean is
   begin
      return C in '0' .. '9';
   end Is_Digit;

   ------------------
   -- Is_Hex_Digit --
   ------------------

   function Is_Hex_Digit (C : Character) return Boolean is
   begin
      return C in '0' .. '9'
        or else C in 'A' .. 'F'
        or else C in 'a' .. 'f';
   end Is_Hex_Digit;

   -----------------
   -- Digit_Value --
   -----------------

   function Digit_Value (C : Character) return Natural is
   begin
      return Character'Pos (C) - Character'Pos ('0');
   end Digit_Value;

   ---------------------
   -- Hex_Digit_Value --
   ---------------------

   function Hex_Digit_Value (C : Character) return Natural is
   begin
      if C in '0' .. '9' then
         return Character'Pos (C) - Character'Pos ('0');
      elsif C in 'A' .. 'F' then
         return Character'Pos (C) - Character'Pos ('A') + 10;
      elsif C in 'a' .. 'f' then
         return Character'Pos (C) - Character'Pos ('a') + 10;
      else
         raise Constraint_Error;
      end if;
   end Hex_Digit_Value;

   ------------------
   -- Scan_Digits --
   ------------------

   procedure Scan_Digits
     (Str   : String;
      Ptr   : in Out Integer;
      Max   : Integer;
      Value : out Natural;
      Count : out Natural)
   is
   begin
      Value := 0;
      Count := 0;

      while Ptr <= Max and then Str (Ptr) in '0' .. '9' loop
         Value := Value * 10 + Digit_Value (Str (Ptr));
         Count := Count + 1;
         Ptr := Ptr + 1;
      end loop;
   end Scan_Digits;

end System.String_Pars;
