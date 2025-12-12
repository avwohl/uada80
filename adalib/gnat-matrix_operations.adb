-- GNAT.Matrix_Operations body for Z80
-- Simple matrix operations implementation

package body GNAT.Matrix_Operations is

   --------------
   -- Identity --
   --------------

   function Identity (Size : Positive) return Matrix is
      Result : Matrix := Zero_Matrix;
   begin
      for I in 1 .. Positive'Min (Size, Max_Rows) loop
         Result (I, I) := 1;
      end loop;
      return Result;
   end Identity;

   ---------
   -- Add --
   ---------

   function Add (A, B : Matrix; Rows, Cols : Positive) return Matrix is
      Result : Matrix := Zero_Matrix;
   begin
      for I in 1 .. Positive'Min (Rows, Max_Rows) loop
         for J in 1 .. Positive'Min (Cols, Max_Cols) loop
            Result (I, J) := A (I, J) + B (I, J);
         end loop;
      end loop;
      return Result;
   end Add;

   --------------
   -- Subtract --
   --------------

   function Subtract (A, B : Matrix; Rows, Cols : Positive) return Matrix is
      Result : Matrix := Zero_Matrix;
   begin
      for I in 1 .. Positive'Min (Rows, Max_Rows) loop
         for J in 1 .. Positive'Min (Cols, Max_Cols) loop
            Result (I, J) := A (I, J) - B (I, J);
         end loop;
      end loop;
      return Result;
   end Subtract;

   --------------
   -- Multiply --
   --------------

   function Multiply (A, B : Matrix;
                      A_Rows, A_Cols, B_Cols : Positive) return Matrix
   is
      Result : Matrix := Zero_Matrix;
      Sum    : Integer;
   begin
      for I in 1 .. Positive'Min (A_Rows, Max_Rows) loop
         for J in 1 .. Positive'Min (B_Cols, Max_Cols) loop
            Sum := 0;
            for K in 1 .. Positive'Min (A_Cols, Max_Cols) loop
               Sum := Sum + A (I, K) * B (K, J);
            end loop;
            Result (I, J) := Sum;
         end loop;
      end loop;
      return Result;
   end Multiply;

   function Multiply (A : Matrix; B : Vector;
                      Rows, Cols : Positive) return Vector
   is
      Result : Vector := Zero_Vector;
      Sum    : Integer;
   begin
      for I in 1 .. Positive'Min (Rows, Max_Rows) loop
         Sum := 0;
         for J in 1 .. Positive'Min (Cols, Max_Cols) loop
            Sum := Sum + A (I, J) * B (J);
         end loop;
         Result (I) := Sum;
      end loop;
      return Result;
   end Multiply;

   ---------------------
   -- Scalar_Multiply --
   ---------------------

   function Scalar_Multiply (A : Matrix; S : Integer;
                             Rows, Cols : Positive) return Matrix
   is
      Result : Matrix := Zero_Matrix;
   begin
      for I in 1 .. Positive'Min (Rows, Max_Rows) loop
         for J in 1 .. Positive'Min (Cols, Max_Cols) loop
            Result (I, J) := A (I, J) * S;
         end loop;
      end loop;
      return Result;
   end Scalar_Multiply;

   ---------------
   -- Transpose --
   ---------------

   function Transpose (A : Matrix; Rows, Cols : Positive) return Matrix is
      Result : Matrix := Zero_Matrix;
   begin
      for I in 1 .. Positive'Min (Rows, Max_Rows) loop
         for J in 1 .. Positive'Min (Cols, Max_Cols) loop
            Result (J, I) := A (I, J);
         end loop;
      end loop;
      return Result;
   end Transpose;

   -------------------
   -- Determinant_2x2 --
   -------------------

   function Determinant_2x2 (A : Matrix) return Integer is
   begin
      return A (1, 1) * A (2, 2) - A (1, 2) * A (2, 1);
   end Determinant_2x2;

   -------------------
   -- Determinant_3x3 --
   -------------------

   function Determinant_3x3 (A : Matrix) return Integer is
   begin
      return A (1, 1) * (A (2, 2) * A (3, 3) - A (2, 3) * A (3, 2))
           - A (1, 2) * (A (2, 1) * A (3, 3) - A (2, 3) * A (3, 1))
           + A (1, 3) * (A (2, 1) * A (3, 2) - A (2, 2) * A (3, 1));
   end Determinant_3x3;

   -----------
   -- Trace --
   -----------

   function Trace (A : Matrix; Size : Positive) return Integer is
      Result : Integer := 0;
   begin
      for I in 1 .. Positive'Min (Size, Max_Rows) loop
         Result := Result + A (I, I);
      end loop;
      return Result;
   end Trace;

   -----------------
   -- Dot_Product --
   -----------------

   function Dot_Product (A, B : Vector; Size : Positive) return Integer is
      Result : Integer := 0;
   begin
      for I in 1 .. Positive'Min (Size, Max_Rows) loop
         Result := Result + A (I) * B (I);
      end loop;
      return Result;
   end Dot_Product;

   ----------------
   -- Vector_Add --
   ----------------

   function Vector_Add (A, B : Vector; Size : Positive) return Vector is
      Result : Vector := Zero_Vector;
   begin
      for I in 1 .. Positive'Min (Size, Max_Rows) loop
         Result (I) := A (I) + B (I);
      end loop;
      return Result;
   end Vector_Add;

   ---------------------
   -- Vector_Subtract --
   ---------------------

   function Vector_Subtract (A, B : Vector; Size : Positive) return Vector is
      Result : Vector := Zero_Vector;
   begin
      for I in 1 .. Positive'Min (Size, Max_Rows) loop
         Result (I) := A (I) - B (I);
      end loop;
      return Result;
   end Vector_Subtract;

   ---------------------
   -- Scalar_Multiply --
   ---------------------

   function Scalar_Multiply (A : Vector; S : Integer;
                             Size : Positive) return Vector
   is
      Result : Vector := Zero_Vector;
   begin
      for I in 1 .. Positive'Min (Size, Max_Rows) loop
         Result (I) := A (I) * S;
      end loop;
      return Result;
   end Scalar_Multiply;

end GNAT.Matrix_Operations;
