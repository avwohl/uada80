-- Ada.Numerics.Big_Numbers.Big_Reals for Z80
-- Arbitrary precision real numbers

with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings.Text_Buffers;

package Ada.Numerics.Big_Numbers.Big_Reals is
   pragma Preelaborate;

   type Big_Real is private;

   function Is_Valid (Arg : Big_Real) return Boolean;

   function "/" (Num, Den : Big_Integers.Big_Integer) return Big_Real;

   function Numerator (Arg : Big_Real) return Big_Integers.Big_Integer;
   function Denominator (Arg : Big_Real) return Big_Integers.Big_Positive;

   function "=" (L, R : Big_Real) return Boolean;
   function "<" (L, R : Big_Real) return Boolean;
   function "<=" (L, R : Big_Real) return Boolean;
   function ">" (L, R : Big_Real) return Boolean;
   function ">=" (L, R : Big_Real) return Boolean;

   function "+" (L : Big_Real) return Big_Real;
   function "-" (L : Big_Real) return Big_Real;
   function "abs" (L : Big_Real) return Big_Real;

   function "+" (L, R : Big_Real) return Big_Real;
   function "-" (L, R : Big_Real) return Big_Real;
   function "*" (L, R : Big_Real) return Big_Real;
   function "/" (L, R : Big_Real) return Big_Real;
   function "**" (L : Big_Real; R : Integer) return Big_Real;

   function Min (L, R : Big_Real) return Big_Real;
   function Max (L, R : Big_Real) return Big_Real;

   function To_Big_Real (Arg : Big_Integers.Big_Integer) return Big_Real;
   function To_Real (Arg : Integer) return Big_Real;
   function To_Real (Arg : Float) return Big_Real;

   function From_String (Arg : String) return Big_Real;
   function To_String (Arg : Big_Real; Fore : Natural := 2; Aft : Natural := 3; Exp : Natural := 0) return String;

   procedure Put_Image (Buffer : in Out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Arg : Big_Real);

   -- Constants
   Zero : constant Big_Real;
   One  : constant Big_Real;

private

   -- Represented as numerator/denominator (rational number)
   type Big_Real is record
      Num : Big_Integers.Big_Integer := Big_Integers.Zero;
      Den : Big_Integers.Big_Integer := Big_Integers.One;
   end record;

   Zero : constant Big_Real := (Num => Big_Integers.Zero, Den => Big_Integers.One);
   One  : constant Big_Real := (Num => Big_Integers.One, Den => Big_Integers.One);

end Ada.Numerics.Big_Numbers.Big_Reals;
