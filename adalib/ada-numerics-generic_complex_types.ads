-- Ada.Numerics.Generic_Complex_Types for Z80
-- Generic complex number types
--
-- Provides complex number support based on a real floating point type

generic
   type Real is digits <>;
package Ada.Numerics.Generic_Complex_Types is
   pragma Pure;

   type Complex is record
      Re : Real'Base;
      Im : Real'Base;
   end record;

   type Imaginary is private;

   -- Imaginary unit
   i : constant Imaginary;
   j : constant Imaginary;

   -- Conversion functions
   function Re (X : Complex) return Real'Base;
   function Im (X : Complex) return Real'Base;
   function Im (X : Imaginary) return Real'Base;

   procedure Set_Re (X : in Out Complex; Re : Real'Base);
   procedure Set_Im (X : in Out Complex; Im : Real'Base);

   -- Compose complex from cartesian coordinates
   function Compose_From_Cartesian (Re : Real'Base) return Complex;
   function Compose_From_Cartesian (Re, Im : Real'Base) return Complex;
   function Compose_From_Cartesian (Im : Imaginary) return Complex;

   -- Modulus and argument
   function Modulus (X : Complex) return Real'Base;
   function "abs" (X : Complex) return Real'Base renames Modulus;
   function Argument (X : Complex) return Real'Base;
   function Argument (X : Complex; Cycle : Real'Base) return Real'Base;

   -- Compose from polar coordinates
   function Compose_From_Polar (Modulus, Argument : Real'Base) return Complex;
   function Compose_From_Polar
     (Modulus, Argument, Cycle : Real'Base) return Complex;

   -- Arithmetic operators
   function "+" (Right : Complex) return Complex;
   function "-" (Right : Complex) return Complex;
   function Conjugate (X : Complex) return Complex;

   function "+" (Left, Right : Complex) return Complex;
   function "-" (Left, Right : Complex) return Complex;
   function "*" (Left, Right : Complex) return Complex;
   function "/" (Left, Right : Complex) return Complex;

   function "**" (Left : Complex; Right : Integer) return Complex;

   -- Mixed Real and Complex operations
   function "+" (Left : Real'Base; Right : Complex) return Complex;
   function "-" (Left : Real'Base; Right : Complex) return Complex;
   function "*" (Left : Real'Base; Right : Complex) return Complex;
   function "/" (Left : Real'Base; Right : Complex) return Complex;

   function "+" (Left : Complex; Right : Real'Base) return Complex;
   function "-" (Left : Complex; Right : Real'Base) return Complex;
   function "*" (Left : Complex; Right : Real'Base) return Complex;
   function "/" (Left : Complex; Right : Real'Base) return Complex;

   -- Imaginary operations
   function "+" (Right : Imaginary) return Imaginary;
   function "-" (Right : Imaginary) return Imaginary;
   function "abs" (Right : Imaginary) return Real'Base;

   function "+" (Left, Right : Imaginary) return Imaginary;
   function "-" (Left, Right : Imaginary) return Imaginary;
   function "*" (Left, Right : Imaginary) return Real'Base;
   function "/" (Left, Right : Imaginary) return Real'Base;

   function "**" (Left : Imaginary; Right : Integer) return Complex;

   -- Mixed Real and Imaginary operations
   function "*" (Left : Real'Base; Right : Imaginary) return Imaginary;
   function "*" (Left : Imaginary; Right : Real'Base) return Imaginary;
   function "/" (Left : Imaginary; Right : Real'Base) return Imaginary;
   function "/" (Left : Real'Base; Right : Imaginary) return Imaginary;

   -- Mixed Complex and Imaginary operations
   function "+" (Left : Complex; Right : Imaginary) return Complex;
   function "-" (Left : Complex; Right : Imaginary) return Complex;
   function "*" (Left : Complex; Right : Imaginary) return Complex;
   function "/" (Left : Complex; Right : Imaginary) return Complex;

   function "+" (Left : Imaginary; Right : Complex) return Complex;
   function "-" (Left : Imaginary; Right : Complex) return Complex;
   function "*" (Left : Imaginary; Right : Complex) return Complex;
   function "/" (Left : Imaginary; Right : Complex) return Complex;

   -- Mixed Real and Imaginary producing Complex
   function "+" (Left : Real'Base; Right : Imaginary) return Complex;
   function "-" (Left : Real'Base; Right : Imaginary) return Complex;

   function "+" (Left : Imaginary; Right : Real'Base) return Complex;
   function "-" (Left : Imaginary; Right : Real'Base) return Complex;

private

   type Imaginary is new Real'Base;

   i : constant Imaginary := 1.0;
   j : constant Imaginary := 1.0;

end Ada.Numerics.Generic_Complex_Types;
