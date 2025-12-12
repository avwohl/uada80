-- GNAT.Matrix_Operations for Z80
-- Simple matrix operations for small matrices

package GNAT.Matrix_Operations is
   pragma Pure;

   Max_Rows : constant := 8;  -- Limited for Z80 memory
   Max_Cols : constant := 8;

   type Matrix is array (1 .. Max_Rows, 1 .. Max_Cols) of Integer;
   type Vector is array (1 .. Max_Rows) of Integer;

   Zero_Matrix : constant Matrix := (others => (others => 0));
   Zero_Vector : constant Vector := (others => 0);

   function Identity (Size : Positive) return Matrix;
   --  Return identity matrix of given size

   function Add (A, B : Matrix; Rows, Cols : Positive) return Matrix;
   --  Add two matrices

   function Subtract (A, B : Matrix; Rows, Cols : Positive) return Matrix;
   --  Subtract two matrices

   function Multiply (A, B : Matrix;
                      A_Rows, A_Cols, B_Cols : Positive) return Matrix;
   --  Multiply two matrices

   function Multiply (A : Matrix; B : Vector;
                      Rows, Cols : Positive) return Vector;
   --  Multiply matrix by vector

   function Scalar_Multiply (A : Matrix; S : Integer;
                             Rows, Cols : Positive) return Matrix;
   --  Multiply matrix by scalar

   function Transpose (A : Matrix; Rows, Cols : Positive) return Matrix;
   --  Transpose matrix

   function Determinant_2x2 (A : Matrix) return Integer;
   --  Determinant of 2x2 matrix

   function Determinant_3x3 (A : Matrix) return Integer;
   --  Determinant of 3x3 matrix

   function Trace (A : Matrix; Size : Positive) return Integer;
   --  Sum of diagonal elements

   function Dot_Product (A, B : Vector; Size : Positive) return Integer;
   --  Dot product of two vectors

   function Vector_Add (A, B : Vector; Size : Positive) return Vector;
   --  Add two vectors

   function Vector_Subtract (A, B : Vector; Size : Positive) return Vector;
   --  Subtract two vectors

   function Scalar_Multiply (A : Vector; S : Integer;
                             Size : Positive) return Vector;
   --  Multiply vector by scalar

end GNAT.Matrix_Operations;
