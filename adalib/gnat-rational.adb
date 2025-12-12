-- GNAT.Rational body for Z80
-- Rational number arithmetic implementation

package body GNAT.Rational is

   ---------
   -- GCD --
   ---------

   function GCD (A, B : Integer) return Integer is
      X : Integer := abs A;
      Y : Integer := abs B;
      T : Integer;
   begin
      if X = 0 then
         return Y;
      end if;
      if Y = 0 then
         return X;
      end if;

      while Y /= 0 loop
         T := Y;
         Y := X mod Y;
         X := T;
      end loop;

      return X;
   end GCD;

   ---------
   -- LCM --
   ---------

   function LCM (A, B : Integer) return Integer is
      G : constant Integer := GCD (A, B);
   begin
      if G = 0 then
         return 0;
      end if;
      return abs (A / G * B);
   end LCM;

   procedure Reduce (R : in Out Rational) is
      G : Integer;
   begin
      if R.Num = 0 then
         R.Den := 1;
         return;
      end if;

      G := GCD (R.Num, R.Den);
      R.Num := R.Num / G;
      R.Den := R.Den / G;

      -- Keep denominator positive
      if R.Den < 0 then
         R.Num := -R.Num;
         R.Den := -R.Den;
      end if;
   end Reduce;

   ----------
   -- Make --
   ----------

   function Make (Numerator : Integer; Denominator : Integer := 1) return Rational is
      R : Rational;
   begin
      if Denominator = 0 then
         R := (Num => 0, Den => 1);  -- Handle divide by zero
      else
         R := (Num => Numerator, Den => Denominator);
         Reduce (R);
      end if;
      return R;
   end Make;

   ---------------
   -- Numerator --
   ---------------

   function Numerator (R : Rational) return Integer is
   begin
      return R.Num;
   end Numerator;

   -----------------
   -- Denominator --
   -----------------

   function Denominator (R : Rational) return Integer is
   begin
      return R.Den;
   end Denominator;

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Rational) return Rational is
   begin
      return Make (Left.Num * Right.Den + Right.Num * Left.Den,
                   Left.Den * Right.Den);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Rational) return Rational is
   begin
      return Make (Left.Num * Right.Den - Right.Num * Left.Den,
                   Left.Den * Right.Den);
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Rational) return Rational is
   begin
      return Make (Left.Num * Right.Num, Left.Den * Right.Den);
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Rational) return Rational is
   begin
      return Make (Left.Num * Right.Den, Left.Den * Right.Num);
   end "/";

   function "-" (R : Rational) return Rational is
   begin
      return (Num => -R.Num, Den => R.Den);
   end "-";

   -----------
   -- "abs" --
   -----------

   function "abs" (R : Rational) return Rational is
   begin
      return (Num => abs R.Num, Den => R.Den);
   end "abs";

   -- Integer arithmetic

   function "+" (Left : Rational; Right : Integer) return Rational is
   begin
      return Left + Make (Right);
   end "+";

   function "+" (Left : Integer; Right : Rational) return Rational is
   begin
      return Make (Left) + Right;
   end "+";

   function "-" (Left : Rational; Right : Integer) return Rational is
   begin
      return Left - Make (Right);
   end "-";

   function "-" (Left : Integer; Right : Rational) return Rational is
   begin
      return Make (Left) - Right;
   end "-";

   function "*" (Left : Rational; Right : Integer) return Rational is
   begin
      return Make (Left.Num * Right, Left.Den);
   end "*";

   function "*" (Left : Integer; Right : Rational) return Rational is
   begin
      return Make (Left * Right.Num, Right.Den);
   end "*";

   function "/" (Left : Rational; Right : Integer) return Rational is
   begin
      return Make (Left.Num, Left.Den * Right);
   end "/";

   function "/" (Left : Integer; Right : Rational) return Rational is
   begin
      return Make (Left * Right.Den, Right.Num);
   end "/";

   -- Comparison

   function "=" (Left, Right : Rational) return Boolean is
   begin
      return Left.Num = Right.Num and Left.Den = Right.Den;
   end "=";

   function "<" (Left, Right : Rational) return Boolean is
   begin
      return Left.Num * Right.Den < Right.Num * Left.Den;
   end "<";

   function ">" (Left, Right : Rational) return Boolean is
   begin
      return Right < Left;
   end ">";

   function "<=" (Left, Right : Rational) return Boolean is
   begin
      return not (Right < Left);
   end "<=";

   function ">=" (Left, Right : Rational) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   -- Predicates

   function Is_Zero (R : Rational) return Boolean is
   begin
      return R.Num = 0;
   end Is_Zero;

   function Is_Positive (R : Rational) return Boolean is
   begin
      return R.Num > 0;
   end Is_Positive;

   function Is_Negative (R : Rational) return Boolean is
   begin
      return R.Num < 0;
   end Is_Negative;

   function Is_Integer (R : Rational) return Boolean is
   begin
      return R.Den = 1;
   end Is_Integer;

   -- Conversion

   function To_Integer (R : Rational) return Integer is
   begin
      return R.Num / R.Den;
   end To_Integer;

   function Round (R : Rational) return Integer is
      Abs_Num : constant Integer := abs R.Num;
      Result  : Integer;
   begin
      Result := Abs_Num / R.Den;
      if (Abs_Num mod R.Den) * 2 >= R.Den then
         Result := Result + 1;
      end if;
      if R.Num < 0 then
         return -Result;
      else
         return Result;
      end if;
   end Round;

   function Floor (R : Rational) return Integer is
      Q : constant Integer := R.Num / R.Den;
   begin
      if R.Num < 0 and R.Num mod R.Den /= 0 then
         return Q - 1;
      else
         return Q;
      end if;
   end Floor;

   function Ceiling (R : Rational) return Integer is
   begin
      return -Floor (-R);
   end Ceiling;

   ------------------
   -- To_Fixed_8_8 --
   ------------------

   function To_Fixed_8_8 (R : Rational) return Integer is
   begin
      return (R.Num * 256) / R.Den;
   end To_Fixed_8_8;

   --------------------
   -- From_Fixed_8_8 --
   --------------------

   function From_Fixed_8_8 (F : Integer) return Rational is
   begin
      return Make (F, 256);
   end From_Fixed_8_8;

   -----------
   -- Image --
   -----------

   function Image (R : Rational) return String is
      function Int_Image (N : Integer) return String is
         Temp : String (1 .. 12);
         Idx  : Natural := 0;
         Val  : Natural;
         Neg  : constant Boolean := N < 0;
      begin
         if N = 0 then
            return "0";
         end if;

         Val := abs N;
         while Val > 0 loop
            Idx := Idx + 1;
            Temp (Idx) := Character'Val (Character'Pos ('0') + Val mod 10);
            Val := Val / 10;
         end loop;

         declare
            Result : String (1 .. Idx + (if Neg then 1 else 0));
            J      : Natural := 0;
         begin
            if Neg then
               J := 1;
               Result (1) := '-';
            end if;
            for I in reverse 1 .. Idx loop
               J := J + 1;
               Result (J) := Temp (I);
            end loop;
            return Result;
         end;
      end Int_Image;

   begin
      if R.Den = 1 then
         return Int_Image (R.Num);
      else
         return Int_Image (R.Num) & "/" & Int_Image (R.Den);
      end if;
   end Image;

   ----------------
   -- Reciprocal --
   ----------------

   function Reciprocal (R : Rational) return Rational is
   begin
      return Make (R.Den, R.Num);
   end Reciprocal;

end GNAT.Rational;
