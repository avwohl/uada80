-- Ada.Numerics.Generic_Complex_Types body for Z80
-- Generic complex number implementation

with Ada.Numerics.Elementary_Functions;

package body Ada.Numerics.Generic_Complex_Types is

   package Math is new Ada.Numerics.Elementary_Functions (Real);

   --------
   -- Re --
   --------

   function Re (X : Complex) return Real'Base is
   begin
      return X.Re;
   end Re;

   --------
   -- Im --
   --------

   function Im (X : Complex) return Real'Base is
   begin
      return X.Im;
   end Im;

   function Im (X : Imaginary) return Real'Base is
   begin
      return Real'Base (X);
   end Im;

   ------------
   -- Set_Re --
   ------------

   procedure Set_Re (X : in Out Complex; Re : Real'Base) is
   begin
      X.Re := Re;
   end Set_Re;

   ------------
   -- Set_Im --
   ------------

   procedure Set_Im (X : in Out Complex; Im : Real'Base) is
   begin
      X.Im := Im;
   end Set_Im;

   ----------------------------
   -- Compose_From_Cartesian --
   ----------------------------

   function Compose_From_Cartesian (Re : Real'Base) return Complex is
   begin
      return (Re => Re, Im => 0.0);
   end Compose_From_Cartesian;

   function Compose_From_Cartesian (Re, Im : Real'Base) return Complex is
   begin
      return (Re => Re, Im => Im);
   end Compose_From_Cartesian;

   function Compose_From_Cartesian (Im : Imaginary) return Complex is
   begin
      return (Re => 0.0, Im => Real'Base (Im));
   end Compose_From_Cartesian;

   -------------
   -- Modulus --
   -------------

   function Modulus (X : Complex) return Real'Base is
   begin
      return Math.Sqrt (X.Re * X.Re + X.Im * X.Im);
   end Modulus;

   --------------
   -- Argument --
   --------------

   function Argument (X : Complex) return Real'Base is
   begin
      return Math.Arctan (X.Im, X.Re);
   end Argument;

   function Argument (X : Complex; Cycle : Real'Base) return Real'Base is
   begin
      return Math.Arctan (X.Im, X.Re, Cycle);
   end Argument;

   ------------------------
   -- Compose_From_Polar --
   ------------------------

   function Compose_From_Polar (Modulus, Argument : Real'Base) return Complex is
   begin
      return (Re => Modulus * Math.Cos (Argument),
              Im => Modulus * Math.Sin (Argument));
   end Compose_From_Polar;

   function Compose_From_Polar
     (Modulus, Argument, Cycle : Real'Base) return Complex
   is
      Angle : constant Real'Base := Argument * 2.0 * Ada.Numerics.Pi / Cycle;
   begin
      return (Re => Modulus * Math.Cos (Angle),
              Im => Modulus * Math.Sin (Angle));
   end Compose_From_Polar;

   ---------
   -- "+" --
   ---------

   function "+" (Right : Complex) return Complex is
   begin
      return Right;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Right : Complex) return Complex is
   begin
      return (Re => -Right.Re, Im => -Right.Im);
   end "-";

   ---------------
   -- Conjugate --
   ---------------

   function Conjugate (X : Complex) return Complex is
   begin
      return (Re => X.Re, Im => -X.Im);
   end Conjugate;

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Complex) return Complex is
   begin
      return (Re => Left.Re + Right.Re, Im => Left.Im + Right.Im);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Complex) return Complex is
   begin
      return (Re => Left.Re - Right.Re, Im => Left.Im - Right.Im);
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Complex) return Complex is
   begin
      return (Re => Left.Re * Right.Re - Left.Im * Right.Im,
              Im => Left.Re * Right.Im + Left.Im * Right.Re);
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Complex) return Complex is
      Denom : constant Real'Base := Right.Re * Right.Re + Right.Im * Right.Im;
   begin
      return (Re => (Left.Re * Right.Re + Left.Im * Right.Im) / Denom,
              Im => (Left.Im * Right.Re - Left.Re * Right.Im) / Denom);
   end "/";

   ----------
   -- "**" --
   ----------

   function "**" (Left : Complex; Right : Integer) return Complex is
      Result : Complex := (Re => 1.0, Im => 0.0);
      Base   : Complex := Left;
      Exp    : Natural := abs Right;
   begin
      while Exp > 0 loop
         if Exp mod 2 = 1 then
            Result := Result * Base;
         end if;
         Base := Base * Base;
         Exp := Exp / 2;
      end loop;

      if Right < 0 then
         return (Re => 1.0, Im => 0.0) / Result;
      else
         return Result;
      end if;
   end "**";

   -- Mixed Real and Complex operations

   function "+" (Left : Real'Base; Right : Complex) return Complex is
   begin
      return (Re => Left + Right.Re, Im => Right.Im);
   end "+";

   function "-" (Left : Real'Base; Right : Complex) return Complex is
   begin
      return (Re => Left - Right.Re, Im => -Right.Im);
   end "-";

   function "*" (Left : Real'Base; Right : Complex) return Complex is
   begin
      return (Re => Left * Right.Re, Im => Left * Right.Im);
   end "*";

   function "/" (Left : Real'Base; Right : Complex) return Complex is
      Denom : constant Real'Base := Right.Re * Right.Re + Right.Im * Right.Im;
   begin
      return (Re => Left * Right.Re / Denom,
              Im => -Left * Right.Im / Denom);
   end "/";

   function "+" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return (Re => Left.Re + Right, Im => Left.Im);
   end "+";

   function "-" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return (Re => Left.Re - Right, Im => Left.Im);
   end "-";

   function "*" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return (Re => Left.Re * Right, Im => Left.Im * Right);
   end "*";

   function "/" (Left : Complex; Right : Real'Base) return Complex is
   begin
      return (Re => Left.Re / Right, Im => Left.Im / Right);
   end "/";

   -- Imaginary operations

   function "+" (Right : Imaginary) return Imaginary is
   begin
      return Right;
   end "+";

   function "-" (Right : Imaginary) return Imaginary is
   begin
      return Imaginary (-Real'Base (Right));
   end "-";

   function "abs" (Right : Imaginary) return Real'Base is
   begin
      return abs Real'Base (Right);
   end "abs";

   function "+" (Left, Right : Imaginary) return Imaginary is
   begin
      return Imaginary (Real'Base (Left) + Real'Base (Right));
   end "+";

   function "-" (Left, Right : Imaginary) return Imaginary is
   begin
      return Imaginary (Real'Base (Left) - Real'Base (Right));
   end "-";

   function "*" (Left, Right : Imaginary) return Real'Base is
   begin
      return -(Real'Base (Left) * Real'Base (Right));
   end "*";

   function "/" (Left, Right : Imaginary) return Real'Base is
   begin
      return Real'Base (Left) / Real'Base (Right);
   end "/";

   function "**" (Left : Imaginary; Right : Integer) return Complex is
   begin
      return (Re => Real'Base (Left), Im => 0.0) ** Right;
   end "**";

   -- Mixed Real and Imaginary operations

   function "*" (Left : Real'Base; Right : Imaginary) return Imaginary is
   begin
      return Imaginary (Left * Real'Base (Right));
   end "*";

   function "*" (Left : Imaginary; Right : Real'Base) return Imaginary is
   begin
      return Imaginary (Real'Base (Left) * Right);
   end "*";

   function "/" (Left : Imaginary; Right : Real'Base) return Imaginary is
   begin
      return Imaginary (Real'Base (Left) / Right);
   end "/";

   function "/" (Left : Real'Base; Right : Imaginary) return Imaginary is
   begin
      return Imaginary (-Left / Real'Base (Right));
   end "/";

   -- Mixed Complex and Imaginary operations

   function "+" (Left : Complex; Right : Imaginary) return Complex is
   begin
      return (Re => Left.Re, Im => Left.Im + Real'Base (Right));
   end "+";

   function "-" (Left : Complex; Right : Imaginary) return Complex is
   begin
      return (Re => Left.Re, Im => Left.Im - Real'Base (Right));
   end "-";

   function "*" (Left : Complex; Right : Imaginary) return Complex is
      Im_R : constant Real'Base := Real'Base (Right);
   begin
      return (Re => -Left.Im * Im_R, Im => Left.Re * Im_R);
   end "*";

   function "/" (Left : Complex; Right : Imaginary) return Complex is
      Im_R : constant Real'Base := Real'Base (Right);
   begin
      return (Re => Left.Im / Im_R, Im => -Left.Re / Im_R);
   end "/";

   function "+" (Left : Imaginary; Right : Complex) return Complex is
   begin
      return (Re => Right.Re, Im => Real'Base (Left) + Right.Im);
   end "+";

   function "-" (Left : Imaginary; Right : Complex) return Complex is
   begin
      return (Re => -Right.Re, Im => Real'Base (Left) - Right.Im);
   end "-";

   function "*" (Left : Imaginary; Right : Complex) return Complex is
      Im_L : constant Real'Base := Real'Base (Left);
   begin
      return (Re => -Im_L * Right.Im, Im => Im_L * Right.Re);
   end "*";

   function "/" (Left : Imaginary; Right : Complex) return Complex is
      Im_L  : constant Real'Base := Real'Base (Left);
      Denom : constant Real'Base := Right.Re * Right.Re + Right.Im * Right.Im;
   begin
      return (Re => Im_L * Right.Im / Denom,
              Im => Im_L * Right.Re / Denom);
   end "/";

   -- Mixed Real and Imaginary producing Complex

   function "+" (Left : Real'Base; Right : Imaginary) return Complex is
   begin
      return (Re => Left, Im => Real'Base (Right));
   end "+";

   function "-" (Left : Real'Base; Right : Imaginary) return Complex is
   begin
      return (Re => Left, Im => -Real'Base (Right));
   end "-";

   function "+" (Left : Imaginary; Right : Real'Base) return Complex is
   begin
      return (Re => Right, Im => Real'Base (Left));
   end "+";

   function "-" (Left : Imaginary; Right : Real'Base) return Complex is
   begin
      return (Re => -Right, Im => Real'Base (Left));
   end "-";

end Ada.Numerics.Generic_Complex_Types;
