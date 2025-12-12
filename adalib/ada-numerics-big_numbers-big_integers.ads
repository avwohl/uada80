-- Ada.Numerics.Big_Numbers.Big_Integers for Z80
-- Arbitrary precision integers (limited for Z80 memory)

with Ada.Strings.Text_Buffers;

package Ada.Numerics.Big_Numbers.Big_Integers is
   pragma Preelaborate;

   -- Maximum digits for Z80 (limited memory)
   Max_Digits : constant := 20;  -- Up to about 60 decimal digits

   type Big_Integer is private;
   type Big_Natural is private;
   type Big_Positive is private;

   function Is_Valid (Arg : Big_Integer) return Boolean;

   function "=" (L, R : Big_Integer) return Boolean;
   function "<" (L, R : Big_Integer) return Boolean;
   function "<=" (L, R : Big_Integer) return Boolean;
   function ">" (L, R : Big_Integer) return Boolean;
   function ">=" (L, R : Big_Integer) return Boolean;

   function "+" (L : Big_Integer) return Big_Integer;
   function "-" (L : Big_Integer) return Big_Integer;
   function "abs" (L : Big_Integer) return Big_Integer;

   function "+" (L, R : Big_Integer) return Big_Integer;
   function "-" (L, R : Big_Integer) return Big_Integer;
   function "*" (L, R : Big_Integer) return Big_Integer;
   function "/" (L, R : Big_Integer) return Big_Integer;
   function "mod" (L, R : Big_Integer) return Big_Integer;
   function "rem" (L, R : Big_Integer) return Big_Integer;
   function "**" (L : Big_Integer; R : Natural) return Big_Integer;

   function Min (L, R : Big_Integer) return Big_Integer;
   function Max (L, R : Big_Integer) return Big_Integer;

   function Greatest_Common_Divisor (L, R : Big_Integer) return Big_Positive;

   function To_Big_Integer (Arg : Integer) return Big_Integer;
   function To_Big_Integer (Arg : Long_Integer) return Big_Integer;

   function To_Integer (Arg : Big_Integer) return Integer;
   function To_Long_Integer (Arg : Big_Integer) return Long_Integer;

   function In_Range (Arg, Low, High : Big_Integer) return Boolean;

   function To_String (Arg : Big_Integer; Width : Natural := 0; Base : Positive := 10) return String;
   function From_String (Arg : String) return Big_Integer;

   procedure Put_Image (Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Arg : Big_Integer);

   -- Constants
   Zero : constant Big_Integer;
   One  : constant Big_Integer;

private

   -- Simple representation: array of digits + sign
   -- Each digit is 0..999 for efficient decimal conversion
   type Digit is range 0 .. 999;
   type Digit_Array is array (1 .. Max_Digits) of Digit;

   type Big_Integer is record
      Digits   : Digit_Array := (others => 0);
      Length   : Natural := 0;  -- Number of significant digits
      Negative : Boolean := False;
   end record;

   type Big_Natural is new Big_Integer;
   type Big_Positive is new Big_Integer;

   Zero : constant Big_Integer := (Digits => (others => 0), Length => 0, Negative => False);
   One  : constant Big_Integer := (Digits => (1 => 1, others => 0), Length => 1, Negative => False);

end Ada.Numerics.Big_Numbers.Big_Integers;
