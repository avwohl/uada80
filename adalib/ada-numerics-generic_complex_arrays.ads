-- Ada.Numerics.Generic_Complex_Arrays for Z80
-- Generic complex array operations
--
-- Provides complex vector and matrix operations

with Ada.Numerics.Generic_Real_Arrays;
with Ada.Numerics.Generic_Complex_Types;

generic
   with package Real_Arrays is new Ada.Numerics.Generic_Real_Arrays (<>);
   use Real_Arrays;
   with package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Real);
   use Complex_Types;
package Ada.Numerics.Generic_Complex_Arrays is
   pragma Pure;

   -- Maximum array size for Z80
   Max_Size : constant := 8;

   -- Complex vector and matrix types
   type Complex_Vector is array (Integer range <>) of Complex;
   type Complex_Matrix is array (Integer range <>, Integer range <>) of Complex;

   -- Vector operations
   function "+" (Right : Complex_Vector) return Complex_Vector;
   function "-" (Right : Complex_Vector) return Complex_Vector;
   function Conjugate (X : Complex_Vector) return Complex_Vector;

   function "+" (Left, Right : Complex_Vector) return Complex_Vector;
   function "-" (Left, Right : Complex_Vector) return Complex_Vector;

   function "*" (Left : Complex; Right : Complex_Vector) return Complex_Vector;
   function "*" (Left : Complex_Vector; Right : Complex) return Complex_Vector;
   function "/" (Left : Complex_Vector; Right : Complex) return Complex_Vector;

   function "*" (Left : Real'Base; Right : Complex_Vector) return Complex_Vector;
   function "*" (Left : Complex_Vector; Right : Real'Base) return Complex_Vector;
   function "/" (Left : Complex_Vector; Right : Real'Base) return Complex_Vector;

   -- Inner product
   function "*" (Left, Right : Complex_Vector) return Complex;

   -- Absolute value (modulus) of each element
   function "abs" (Right : Complex_Vector) return Real_Vector;

   -- Unit vector
   function Unit_Vector
     (Index : Integer;
      Order : Positive;
      First : Integer := 1) return Complex_Vector;

   -- Matrix operations
   function "+" (Right : Complex_Matrix) return Complex_Matrix;
   function "-" (Right : Complex_Matrix) return Complex_Matrix;
   function Conjugate (X : Complex_Matrix) return Complex_Matrix;
   function Transpose (X : Complex_Matrix) return Complex_Matrix;

   function "+" (Left, Right : Complex_Matrix) return Complex_Matrix;
   function "-" (Left, Right : Complex_Matrix) return Complex_Matrix;
   function "*" (Left, Right : Complex_Matrix) return Complex_Matrix;

   function "*" (Left : Complex; Right : Complex_Matrix) return Complex_Matrix;
   function "*" (Left : Complex_Matrix; Right : Complex) return Complex_Matrix;
   function "/" (Left : Complex_Matrix; Right : Complex) return Complex_Matrix;

   function "*" (Left : Real'Base; Right : Complex_Matrix) return Complex_Matrix;
   function "*" (Left : Complex_Matrix; Right : Real'Base) return Complex_Matrix;
   function "/" (Left : Complex_Matrix; Right : Real'Base) return Complex_Matrix;

   -- Matrix-vector multiplication
   function "*" (Left : Complex_Matrix; Right : Complex_Vector) return Complex_Vector;
   function "*" (Left : Complex_Vector; Right : Complex_Matrix) return Complex_Vector;

   -- Outer product
   function "*" (Left, Right : Complex_Vector) return Complex_Matrix;

   -- Identity matrix
   function Unit_Matrix
     (Order            : Positive;
      First_1, First_2 : Integer := 1) return Complex_Matrix;

   -- Conversions between real and complex arrays
   function Compose_From_Cartesian (Re : Real_Vector) return Complex_Vector;
   function Compose_From_Cartesian (Re, Im : Real_Vector) return Complex_Vector;
   function Compose_From_Cartesian (Re : Real_Matrix) return Complex_Matrix;
   function Compose_From_Cartesian (Re, Im : Real_Matrix) return Complex_Matrix;

   function Re (X : Complex_Vector) return Real_Vector;
   function Im (X : Complex_Vector) return Real_Vector;
   function Re (X : Complex_Matrix) return Real_Matrix;
   function Im (X : Complex_Matrix) return Real_Matrix;

   procedure Set_Re (X : in Out Complex_Vector; Re : Real_Vector);
   procedure Set_Im (X : in Out Complex_Vector; Im : Real_Vector);
   procedure Set_Re (X : in Out Complex_Matrix; Re : Real_Matrix);
   procedure Set_Im (X : in Out Complex_Matrix; Im : Real_Matrix);

end Ada.Numerics.Generic_Complex_Arrays;
