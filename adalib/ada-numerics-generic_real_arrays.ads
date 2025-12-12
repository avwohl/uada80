-- Ada.Numerics.Generic_Real_Arrays for Z80
-- Generic real vector and matrix operations

generic
   type Real is digits <>;
package Ada.Numerics.Generic_Real_Arrays is
   pragma Preelaborate;

   -- Vector and Matrix types
   type Real_Vector is array (Integer range <>) of Real'Base;
   type Real_Matrix is array (Integer range <>, Integer range <>) of Real'Base;

   -- Vector operations
   function "+" (Right : Real_Vector) return Real_Vector;
   function "-" (Right : Real_Vector) return Real_Vector;
   function "abs" (Right : Real_Vector) return Real_Vector;

   function "+" (Left, Right : Real_Vector) return Real_Vector;
   function "-" (Left, Right : Real_Vector) return Real_Vector;

   function "*" (Left, Right : Real_Vector) return Real'Base;  -- Dot product
   function "*" (Left : Real'Base; Right : Real_Vector) return Real_Vector;
   function "*" (Left : Real_Vector; Right : Real'Base) return Real_Vector;
   function "/" (Left : Real_Vector; Right : Real'Base) return Real_Vector;

   -- Matrix operations
   function "+" (Right : Real_Matrix) return Real_Matrix;
   function "-" (Right : Real_Matrix) return Real_Matrix;
   function "abs" (Right : Real_Matrix) return Real_Matrix;

   function "+" (Left, Right : Real_Matrix) return Real_Matrix;
   function "-" (Left, Right : Real_Matrix) return Real_Matrix;

   function "*" (Left, Right : Real_Matrix) return Real_Matrix;
   function "*" (Left : Real_Matrix; Right : Real_Vector) return Real_Vector;
   function "*" (Left : Real_Vector; Right : Real_Matrix) return Real_Vector;

   function "*" (Left : Real'Base; Right : Real_Matrix) return Real_Matrix;
   function "*" (Left : Real_Matrix; Right : Real'Base) return Real_Matrix;
   function "/" (Left : Real_Matrix; Right : Real'Base) return Real_Matrix;

   -- Matrix transpose
   function Transpose (X : Real_Matrix) return Real_Matrix;

   -- Solving linear systems (simplified for Z80)
   function Solve (A : Real_Matrix; X : Real_Vector) return Real_Vector;
   function Solve (A, X : Real_Matrix) return Real_Matrix;

   -- Vector and matrix norms
   function Unit_Vector
     (Index : Integer;
      Order : Positive;
      First : Integer := 1) return Real_Vector;

   function Unit_Matrix
     (Order   : Positive;
      First_1 : Integer := 1;
      First_2 : Integer := 1) return Real_Matrix;

end Ada.Numerics.Generic_Real_Arrays;
