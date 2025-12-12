-- Ada.Numerics.Big_Numbers.Big_Reals body for Z80
-- Arbitrary precision real numbers implementation

package body Ada.Numerics.Big_Numbers.Big_Reals is

   use Big_Integers;

   procedure Normalize (R : in out Big_Real) is
      G : Big_Positive;
   begin
      if R.Num = Big_Integers.Zero then
         R.Den := Big_Integers.One;
      else
         G := Greatest_Common_Divisor (R.Num, R.Den);
         R.Num := R.Num / Big_Integer (G);
         R.Den := R.Den / Big_Integer (G);
      end if;
   end Normalize;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Arg : Big_Real) return Boolean is
   begin
      return Is_Valid (Arg.Num) and Is_Valid (Arg.Den) and Arg.Den /= Big_Integers.Zero;
   end Is_Valid;

   ---------
   -- "/" --
   ---------

   function "/" (Num, Den : Big_Integers.Big_Integer) return Big_Real is
      Result : Big_Real;
   begin
      if Den = Big_Integers.Zero then
         raise Constraint_Error;
      end if;
      Result.Num := Num;
      Result.Den := Den;
      Normalize (Result);
      return Result;
   end "/";

   ---------------
   -- Numerator --
   ---------------

   function Numerator (Arg : Big_Real) return Big_Integers.Big_Integer is
   begin
      return Arg.Num;
   end Numerator;

   -----------------
   -- Denominator --
   -----------------

   function Denominator (Arg : Big_Real) return Big_Integers.Big_Positive is
   begin
      return Big_Integers.Big_Positive (Arg.Den);
   end Denominator;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Big_Real) return Boolean is
   begin
      return L.Num * R.Den = R.Num * L.Den;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Big_Real) return Boolean is
   begin
      return L.Num * R.Den < R.Num * L.Den;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (L, R : Big_Real) return Boolean is
   begin
      return L < R or L = R;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (L, R : Big_Real) return Boolean is
   begin
      return R < L;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (L, R : Big_Real) return Boolean is
   begin
      return R < L or L = R;
   end ">=";

   ---------
   -- "+" --
   ---------

   function "+" (L : Big_Real) return Big_Real is
   begin
      return L;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L : Big_Real) return Big_Real is
      Result : Big_Real;
   begin
      Result.Num := -L.Num;
      Result.Den := L.Den;
      return Result;
   end "-";

   -----------
   -- "abs" --
   -----------

   function "abs" (L : Big_Real) return Big_Real is
      Result : Big_Real;
   begin
      Result.Num := abs L.Num;
      Result.Den := L.Den;
      return Result;
   end "abs";

   ---------
   -- "+" --
   ---------

   function "+" (L, R : Big_Real) return Big_Real is
      Result : Big_Real;
   begin
      Result.Num := L.Num * R.Den + R.Num * L.Den;
      Result.Den := L.Den * R.Den;
      Normalize (Result);
      return Result;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L, R : Big_Real) return Big_Real is
      Result : Big_Real;
   begin
      Result.Num := L.Num * R.Den - R.Num * L.Den;
      Result.Den := L.Den * R.Den;
      Normalize (Result);
      return Result;
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (L, R : Big_Real) return Big_Real is
      Result : Big_Real;
   begin
      Result.Num := L.Num * R.Num;
      Result.Den := L.Den * R.Den;
      Normalize (Result);
      return Result;
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (L, R : Big_Real) return Big_Real is
      Result : Big_Real;
   begin
      if R.Num = Big_Integers.Zero then
         raise Constraint_Error;
      end if;
      Result.Num := L.Num * R.Den;
      Result.Den := L.Den * R.Num;
      Normalize (Result);
      return Result;
   end "/";

   ----------
   -- "**" --
   ----------

   function "**" (L : Big_Real; R : Integer) return Big_Real is
      Result : Big_Real := One;
      Base   : Big_Real := L;
      Exp    : Natural := abs R;
   begin
      while Exp > 0 loop
         if Exp mod 2 = 1 then
            Result := Result * Base;
         end if;
         Base := Base * Base;
         Exp := Exp / 2;
      end loop;

      if R < 0 then
         Result := One / Result;
      end if;

      return Result;
   end "**";

   ---------
   -- Min --
   ---------

   function Min (L, R : Big_Real) return Big_Real is
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

   function Max (L, R : Big_Real) return Big_Real is
   begin
      if L > R then
         return L;
      else
         return R;
      end if;
   end Max;

   -----------------
   -- To_Big_Real --
   -----------------

   function To_Big_Real (Arg : Big_Integers.Big_Integer) return Big_Real is
      Result : Big_Real;
   begin
      Result.Num := Arg;
      Result.Den := Big_Integers.One;
      return Result;
   end To_Big_Real;

   -------------
   -- To_Real --
   -------------

   function To_Real (Arg : Integer) return Big_Real is
   begin
      return To_Big_Real (To_Big_Integer (Arg));
   end To_Real;

   function To_Real (Arg : Float) return Big_Real is
      Result   : Big_Real;
      Int_Part : Integer := Integer (Arg);
      Frac     : Float := abs (Arg - Float (Int_Part));
      Scale    : Integer := 1;
   begin
      -- Simple conversion: multiply by power of 10 until fractional part is integer
      for I in 1 .. 6 loop  -- Up to 6 decimal places
         exit when Frac = 0.0;
         Frac := Frac * 10.0;
         Scale := Scale * 10;
      end loop;

      Result.Num := To_Big_Integer (Int_Part * Scale + Integer (Frac));
      Result.Den := To_Big_Integer (Scale);

      if Arg < 0.0 and Result.Num /= Big_Integers.Zero then
         Result.Num := -Result.Num;
      end if;

      Normalize (Result);
      return Result;
   end To_Real;

   -----------------
   -- From_String --
   -----------------

   function From_String (Arg : String) return Big_Real is
      Result    : Big_Real;
      Dot_Pos   : Natural := 0;
      Scale     : Big_Integer := Big_Integers.One;
      Ten       : constant Big_Integer := To_Big_Integer (10);
   begin
      -- Find decimal point
      for I in Arg'Range loop
         if Arg (I) = '.' then
            Dot_Pos := I;
            exit;
         end if;
      end loop;

      if Dot_Pos = 0 then
         -- No decimal point: integer
         Result.Num := Big_Integers.From_String (Arg);
         Result.Den := Big_Integers.One;
      else
         -- Has decimal point
         declare
            Int_Str  : constant String := Arg (Arg'First .. Dot_Pos - 1);
            Frac_Str : constant String := Arg (Dot_Pos + 1 .. Arg'Last);
            Frac_Len : constant Natural := Frac_Str'Length;
         begin
            Result.Num := Big_Integers.From_String (Int_Str);

            -- Calculate scale (10^frac_len)
            for I in 1 .. Frac_Len loop
               Scale := Scale * Ten;
            end loop;

            Result.Num := Result.Num * Scale + Big_Integers.From_String (Frac_Str);
            Result.Den := Scale;
         end;
      end if;

      Normalize (Result);
      return Result;
   end From_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Arg : Big_Real; Fore : Natural := 2; Aft : Natural := 3; Exp : Natural := 0) return String is
      pragma Unreferenced (Fore, Exp);
      Num      : Big_Integer := abs Arg.Num;
      Int_Part : Big_Integer;
      Frac_Num : Big_Integer;
      Ten      : constant Big_Integer := To_Big_Integer (10);
      Scale    : Big_Integer := Big_Integers.One;
      Result   : String (1 .. 50);
      Pos      : Natural := 0;
   begin
      -- Calculate scale for desired decimal places
      for I in 1 .. Aft loop
         Scale := Scale * Ten;
      end loop;

      -- Integer part
      Int_Part := Num / Arg.Den;

      -- Fractional part scaled up
      Frac_Num := ((Num - Int_Part * Arg.Den) * Scale) / Arg.Den;

      -- Build result string
      declare
         Int_Str  : constant String := Big_Integers.To_String (Int_Part);
         Frac_Str : constant String := Big_Integers.To_String (Frac_Num);
      begin
         if Arg.Num < Big_Integers.Zero then
            Pos := Pos + 1;
            Result (Pos) := '-';
         end if;

         for I in Int_Str'Range loop
            Pos := Pos + 1;
            Result (Pos) := Int_Str (I);
         end loop;

         if Aft > 0 then
            Pos := Pos + 1;
            Result (Pos) := '.';

            -- Pad with leading zeros if needed
            for I in 1 .. Aft - Frac_Str'Length loop
               Pos := Pos + 1;
               Result (Pos) := '0';
            end loop;

            for I in Frac_Str'Range loop
               exit when Pos >= Result'Last;
               Pos := Pos + 1;
               Result (Pos) := Frac_Str (I);
            end loop;
         end if;
      end;

      return Result (1 .. Pos);
   end To_String;

   ---------------
   -- Put_Image --
   ---------------

   procedure Put_Image (Buffer : in Out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Arg : Big_Real) is
   begin
      Buffer.Put (To_String (Arg));
   end Put_Image;

end Ada.Numerics.Big_Numbers.Big_Reals;
