-- Ada.Numerics.Big_Numbers.Big_Integers body for Z80
-- Arbitrary precision integers implementation

package body Ada.Numerics.Big_Numbers.Big_Integers is

   Base : constant := 1000;  -- Each digit is 0..999

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Arg : Big_Integer) return Boolean is
   begin
      return Arg.Length <= Max_Digits;
   end Is_Valid;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Big_Integer) return Boolean is
   begin
      if L.Negative /= R.Negative then
         return L.Length = 0 and R.Length = 0;  -- Both zero
      end if;
      if L.Length /= R.Length then
         return False;
      end if;
      for I in 1 .. L.Length loop
         if L.Digits (I) /= R.Digits (I) then
            return False;
         end if;
      end loop;
      return True;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Big_Integer) return Boolean is
   begin
      if L.Negative and not R.Negative then
         return True;
      end if;
      if not L.Negative and R.Negative then
         return False;
      end if;

      -- Both same sign
      if L.Negative then
         -- Both negative: larger magnitude is smaller
         if L.Length > R.Length then
            return True;
         elsif L.Length < R.Length then
            return False;
         end if;
         for I in reverse 1 .. L.Length loop
            if L.Digits (I) > R.Digits (I) then
               return True;
            elsif L.Digits (I) < R.Digits (I) then
               return False;
            end if;
         end loop;
      else
         -- Both positive
         if L.Length < R.Length then
            return True;
         elsif L.Length > R.Length then
            return False;
         end if;
         for I in reverse 1 .. L.Length loop
            if L.Digits (I) < R.Digits (I) then
               return True;
            elsif L.Digits (I) > R.Digits (I) then
               return False;
            end if;
         end loop;
      end if;
      return False;  -- Equal
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (L, R : Big_Integer) return Boolean is
   begin
      return L < R or L = R;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (L, R : Big_Integer) return Boolean is
   begin
      return R < L;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (L, R : Big_Integer) return Boolean is
   begin
      return R < L or L = R;
   end ">=";

   ---------
   -- "+" --
   ---------

   function "+" (L : Big_Integer) return Big_Integer is
   begin
      return L;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L : Big_Integer) return Big_Integer is
      Result : Big_Integer := L;
   begin
      if Result.Length > 0 then
         Result.Negative := not Result.Negative;
      end if;
      return Result;
   end "-";

   -----------
   -- "abs" --
   -----------

   function "abs" (L : Big_Integer) return Big_Integer is
      Result : Big_Integer := L;
   begin
      Result.Negative := False;
      return Result;
   end "abs";

   ---------
   -- "+" --
   ---------

   function "+" (L, R : Big_Integer) return Big_Integer is
      Result : Big_Integer;
      Carry  : Natural := 0;
      Sum    : Natural;
   begin
      if L.Negative = R.Negative then
         -- Same sign: add magnitudes
         Result.Length := Natural'Max (L.Length, R.Length);
         Result.Negative := L.Negative;

         for I in 1 .. Result.Length loop
            Sum := Carry;
            if I <= L.Length then
               Sum := Sum + Natural (L.Digits (I));
            end if;
            if I <= R.Length then
               Sum := Sum + Natural (R.Digits (I));
            end if;
            Result.Digits (I) := Digit (Sum mod Base);
            Carry := Sum / Base;
         end loop;

         if Carry > 0 and Result.Length < Max_Digits then
            Result.Length := Result.Length + 1;
            Result.Digits (Result.Length) := Digit (Carry);
         end if;
      else
         -- Different signs: subtract
         if L.Negative then
            Result := R - (abs L);
         else
            Result := L - (abs R);
         end if;
      end if;

      return Result;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L, R : Big_Integer) return Big_Integer is
      Result   : Big_Integer;
      Borrow   : Integer := 0;
      Diff     : Integer;
      L_Bigger : Boolean;
   begin
      if L.Negative /= R.Negative then
         -- Different signs: add magnitudes
         if L.Negative then
            Result := -(abs L + abs R);
         else
            Result := abs L + abs R;
         end if;
         return Result;
      end if;

      -- Same sign: subtract magnitudes
      L_Bigger := abs L >= abs R;

      if L_Bigger then
         Result.Length := L.Length;
         for I in 1 .. Result.Length loop
            Diff := -Borrow;
            if I <= L.Length then
               Diff := Diff + Integer (L.Digits (I));
            end if;
            if I <= R.Length then
               Diff := Diff - Integer (R.Digits (I));
            end if;
            if Diff < 0 then
               Diff := Diff + Base;
               Borrow := 1;
            else
               Borrow := 0;
            end if;
            Result.Digits (I) := Digit (Diff);
         end loop;
         Result.Negative := L.Negative;
      else
         Result.Length := R.Length;
         for I in 1 .. Result.Length loop
            Diff := -Borrow;
            if I <= R.Length then
               Diff := Diff + Integer (R.Digits (I));
            end if;
            if I <= L.Length then
               Diff := Diff - Integer (L.Digits (I));
            end if;
            if Diff < 0 then
               Diff := Diff + Base;
               Borrow := 1;
            else
               Borrow := 0;
            end if;
            Result.Digits (I) := Digit (Diff);
         end loop;
         Result.Negative := not L.Negative;
      end if;

      -- Normalize: remove leading zeros
      while Result.Length > 0 and then Result.Digits (Result.Length) = 0 loop
         Result.Length := Result.Length - 1;
      end loop;

      if Result.Length = 0 then
         Result.Negative := False;
      end if;

      return Result;
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (L, R : Big_Integer) return Big_Integer is
      Result  : Big_Integer;
      Product : Natural;
      Carry   : Natural;
   begin
      if L.Length = 0 or R.Length = 0 then
         return Zero;
      end if;

      Result.Negative := L.Negative /= R.Negative;
      Result.Length := Natural'Min (L.Length + R.Length, Max_Digits);

      for I in 1 .. L.Length loop
         Carry := 0;
         for J in 1 .. R.Length loop
            if I + J - 1 <= Max_Digits then
               Product := Natural (L.Digits (I)) * Natural (R.Digits (J)) +
                          Natural (Result.Digits (I + J - 1)) + Carry;
               Result.Digits (I + J - 1) := Digit (Product mod Base);
               Carry := Product / Base;
            end if;
         end loop;
         if I + R.Length <= Max_Digits and Carry > 0 then
            Result.Digits (I + R.Length) := Digit (
               Natural (Result.Digits (I + R.Length)) + Carry);
         end if;
      end loop;

      -- Normalize
      while Result.Length > 0 and then Result.Digits (Result.Length) = 0 loop
         Result.Length := Result.Length - 1;
      end loop;

      if Result.Length = 0 then
         Result.Negative := False;
      end if;

      return Result;
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (L, R : Big_Integer) return Big_Integer is
      Result    : Big_Integer;
      Remainder : Big_Integer := abs L;
      Divisor   : Big_Integer := abs R;
   begin
      if R.Length = 0 then
         raise Constraint_Error;  -- Division by zero
      end if;

      if Remainder < Divisor then
         return Zero;
      end if;

      -- Simple repeated subtraction for Z80 (not efficient but correct)
      while Remainder >= Divisor loop
         Remainder := Remainder - Divisor;
         Result := Result + One;
      end loop;

      Result.Negative := L.Negative /= R.Negative;
      if Result.Length = 0 then
         Result.Negative := False;
      end if;

      return Result;
   end "/";

   -----------
   -- "mod" --
   -----------

   function "mod" (L, R : Big_Integer) return Big_Integer is
      Result : Big_Integer;
   begin
      Result := L - (L / R) * R;
      if Result.Negative and R.Length > 0 then
         if R.Negative then
            Result := Result - R;
         else
            Result := Result + R;
         end if;
      end if;
      return Result;
   end "mod";

   -----------
   -- "rem" --
   -----------

   function "rem" (L, R : Big_Integer) return Big_Integer is
   begin
      return L - (L / R) * R;
   end "rem";

   ----------
   -- "**" --
   ----------

   function "**" (L : Big_Integer; R : Natural) return Big_Integer is
      Result : Big_Integer := One;
      Base   : Big_Integer := L;
      Exp    : Natural := R;
   begin
      while Exp > 0 loop
         if Exp mod 2 = 1 then
            Result := Result * Base;
         end if;
         Base := Base * Base;
         Exp := Exp / 2;
      end loop;
      return Result;
   end "**";

   ---------
   -- Min --
   ---------

   function Min (L, R : Big_Integer) return Big_Integer is
   begin
      if L < R then
         return L;
      else
         return R;
      end if;
   end Min;

   ---------
   -- Max --
   ---------

   function Max (L, R : Big_Integer) return Big_Integer is
   begin
      if L > R then
         return L;
      else
         return R;
      end if;
   end Max;

   ------------------------------
   -- Greatest_Common_Divisor --
   ------------------------------

   function Greatest_Common_Divisor (L, R : Big_Integer) return Big_Positive is
      A, B, T : Big_Integer;
   begin
      A := abs L;
      B := abs R;

      while B.Length > 0 loop
         T := B;
         B := A - (A / B) * B;
         A := T;
      end loop;

      return Big_Positive (A);
   end Greatest_Common_Divisor;

   --------------------
   -- To_Big_Integer --
   --------------------

   function To_Big_Integer (Arg : Integer) return Big_Integer is
      Result : Big_Integer;
      Val    : Integer := abs Arg;
   begin
      Result.Negative := Arg < 0;
      while Val > 0 loop
         Result.Length := Result.Length + 1;
         Result.Digits (Result.Length) := Digit (Val mod Base);
         Val := Val / Base;
      end loop;
      return Result;
   end To_Big_Integer;

   function To_Big_Integer (Arg : Long_Integer) return Big_Integer is
      Result : Big_Integer;
      Val    : Long_Integer := abs Arg;
   begin
      Result.Negative := Arg < 0;
      while Val > 0 loop
         Result.Length := Result.Length + 1;
         Result.Digits (Result.Length) := Digit (Integer (Val mod Long_Integer (Base)));
         Val := Val / Long_Integer (Base);
      end loop;
      return Result;
   end To_Big_Integer;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Arg : Big_Integer) return Integer is
      Result : Integer := 0;
      Mult   : Integer := 1;
   begin
      for I in 1 .. Arg.Length loop
         Result := Result + Integer (Arg.Digits (I)) * Mult;
         Mult := Mult * Base;
      end loop;
      if Arg.Negative then
         Result := -Result;
      end if;
      return Result;
   end To_Integer;

   ---------------------
   -- To_Long_Integer --
   ---------------------

   function To_Long_Integer (Arg : Big_Integer) return Long_Integer is
      Result : Long_Integer := 0;
      Mult   : Long_Integer := 1;
   begin
      for I in 1 .. Arg.Length loop
         Result := Result + Long_Integer (Arg.Digits (I)) * Mult;
         Mult := Mult * Long_Integer (Base);
      end loop;
      if Arg.Negative then
         Result := -Result;
      end if;
      return Result;
   end To_Long_Integer;

   --------------
   -- In_Range --
   --------------

   function In_Range (Arg, Low, High : Big_Integer) return Boolean is
   begin
      return Arg >= Low and Arg <= High;
   end In_Range;

   ---------------
   -- To_String --
   ---------------

   function To_String (Arg : Big_Integer; Width : Natural := 0; Base : Positive := 10) return String is
      pragma Unreferenced (Base);  -- Only base 10 for simplicity
      Max_Str : constant := Max_Digits * 3 + 1;
      Buffer  : String (1 .. Max_Str);
      Pos     : Natural := Max_Str;
      Temp    : Big_Integer := abs Arg;
      D       : Natural;
   begin
      if Temp.Length = 0 then
         Buffer (Pos) := '0';
         Pos := Pos - 1;
      else
         while Temp.Length > 0 loop
            -- Extract least significant digit group
            D := Natural (Temp.Digits (1));
            -- Shift down
            for I in 1 .. Temp.Length - 1 loop
               Temp.Digits (I) := Temp.Digits (I + 1);
            end loop;
            Temp.Length := Temp.Length - 1;

            -- Output up to 3 digits
            for J in 1 .. 3 loop
               exit when D = 0 and Temp.Length = 0;
               Buffer (Pos) := Character'Val (Character'Pos ('0') + (D mod 10));
               Pos := Pos - 1;
               D := D / 10;
            end loop;
         end loop;
      end if;

      if Arg.Negative then
         Buffer (Pos) := '-';
         Pos := Pos - 1;
      end if;

      declare
         Result : constant String := Buffer (Pos + 1 .. Max_Str);
      begin
         if Result'Length >= Width then
            return Result;
         else
            return (1 .. Width - Result'Length => ' ') & Result;
         end if;
      end;
   end To_String;

   -----------------
   -- From_String --
   -----------------

   function From_String (Arg : String) return Big_Integer is
      Result   : Big_Integer;
      Negative : Boolean := False;
      Start    : Positive := Arg'First;
   begin
      -- Skip leading whitespace
      while Start <= Arg'Last and then Arg (Start) = ' ' loop
         Start := Start + 1;
      end loop;

      if Start > Arg'Last then
         return Zero;
      end if;

      -- Check for sign
      if Arg (Start) = '-' then
         Negative := True;
         Start := Start + 1;
      elsif Arg (Start) = '+' then
         Start := Start + 1;
      end if;

      -- Parse digits
      for I in Start .. Arg'Last loop
         if Arg (I) in '0' .. '9' then
            Result := Result * To_Big_Integer (10) +
                     To_Big_Integer (Character'Pos (Arg (I)) - Character'Pos ('0'));
         else
            exit;
         end if;
      end loop;

      Result.Negative := Negative and Result.Length > 0;
      return Result;
   end From_String;

   ---------------
   -- Put_Image --
   ---------------

   procedure Put_Image (Buffer : in Out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Arg : Big_Integer) is
   begin
      Buffer.Put (To_String (Arg));
   end Put_Image;

end Ada.Numerics.Big_Numbers.Big_Integers;
