-- Ada.Numerics.Generic_Real_Arrays body for Z80
-- Generic real vector and matrix operations implementation

package body Ada.Numerics.Generic_Real_Arrays is

   ---------
   -- "+" --
   ---------

   function "+" (Right : Real_Vector) return Real_Vector is
      Result : Real_Vector (Right'Range);
   begin
      for I in Right'Range loop
         Result (I) := Right (I);
      end loop;
      return Result;
   end "+";

   function "-" (Right : Real_Vector) return Real_Vector is
      Result : Real_Vector (Right'Range);
   begin
      for I in Right'Range loop
         Result (I) := -Right (I);
      end loop;
      return Result;
   end "-";

   function "abs" (Right : Real_Vector) return Real_Vector is
      Result : Real_Vector (Right'Range);
   begin
      for I in Right'Range loop
         Result (I) := abs Right (I);
      end loop;
      return Result;
   end "abs";

   function "+" (Left, Right : Real_Vector) return Real_Vector is
      Result : Real_Vector (Left'Range);
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error;
      end if;
      for I in Left'Range loop
         Result (I) := Left (I) + Right (Right'First + I - Left'First);
      end loop;
      return Result;
   end "+";

   function "-" (Left, Right : Real_Vector) return Real_Vector is
      Result : Real_Vector (Left'Range);
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error;
      end if;
      for I in Left'Range loop
         Result (I) := Left (I) - Right (Right'First + I - Left'First);
      end loop;
      return Result;
   end "-";

   function "*" (Left, Right : Real_Vector) return Real'Base is
      Sum : Real'Base := 0.0;
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error;
      end if;
      for I in Left'Range loop
         Sum := Sum + Left (I) * Right (Right'First + I - Left'First);
      end loop;
      return Sum;
   end "*";

   function "*" (Left : Real'Base; Right : Real_Vector) return Real_Vector is
      Result : Real_Vector (Right'Range);
   begin
      for I in Right'Range loop
         Result (I) := Left * Right (I);
      end loop;
      return Result;
   end "*";

   function "*" (Left : Real_Vector; Right : Real'Base) return Real_Vector is
      Result : Real_Vector (Left'Range);
   begin
      for I in Left'Range loop
         Result (I) := Left (I) * Right;
      end loop;
      return Result;
   end "*";

   function "/" (Left : Real_Vector; Right : Real'Base) return Real_Vector is
      Result : Real_Vector (Left'Range);
   begin
      for I in Left'Range loop
         Result (I) := Left (I) / Right;
      end loop;
      return Result;
   end "/";

   -- Matrix operations

   function "+" (Right : Real_Matrix) return Real_Matrix is
      Result : Real_Matrix (Right'Range (1), Right'Range (2));
   begin
      for I in Right'Range (1) loop
         for J in Right'Range (2) loop
            Result (I, J) := Right (I, J);
         end loop;
      end loop;
      return Result;
   end "+";

   function "-" (Right : Real_Matrix) return Real_Matrix is
      Result : Real_Matrix (Right'Range (1), Right'Range (2));
   begin
      for I in Right'Range (1) loop
         for J in Right'Range (2) loop
            Result (I, J) := -Right (I, J);
         end loop;
      end loop;
      return Result;
   end "-";

   function "abs" (Right : Real_Matrix) return Real_Matrix is
      Result : Real_Matrix (Right'Range (1), Right'Range (2));
   begin
      for I in Right'Range (1) loop
         for J in Right'Range (2) loop
            Result (I, J) := abs Right (I, J);
         end loop;
      end loop;
      return Result;
   end "abs";

   function "+" (Left, Right : Real_Matrix) return Real_Matrix is
      Result : Real_Matrix (Left'Range (1), Left'Range (2));
   begin
      if Left'Length (1) /= Right'Length (1) or Left'Length (2) /= Right'Length (2) then
         raise Constraint_Error;
      end if;
      for I in Left'Range (1) loop
         for J in Left'Range (2) loop
            Result (I, J) := Left (I, J) +
              Right (Right'First (1) + I - Left'First (1),
                     Right'First (2) + J - Left'First (2));
         end loop;
      end loop;
      return Result;
   end "+";

   function "-" (Left, Right : Real_Matrix) return Real_Matrix is
      Result : Real_Matrix (Left'Range (1), Left'Range (2));
   begin
      if Left'Length (1) /= Right'Length (1) or Left'Length (2) /= Right'Length (2) then
         raise Constraint_Error;
      end if;
      for I in Left'Range (1) loop
         for J in Left'Range (2) loop
            Result (I, J) := Left (I, J) -
              Right (Right'First (1) + I - Left'First (1),
                     Right'First (2) + J - Left'First (2));
         end loop;
      end loop;
      return Result;
   end "-";

   function "*" (Left, Right : Real_Matrix) return Real_Matrix is
      Result : Real_Matrix (Left'Range (1), Right'Range (2));
   begin
      if Left'Length (2) /= Right'Length (1) then
         raise Constraint_Error;
      end if;
      for I in Result'Range (1) loop
         for J in Result'Range (2) loop
            Result (I, J) := 0.0;
            for K in Left'Range (2) loop
               Result (I, J) := Result (I, J) +
                 Left (I, K) * Right (Right'First (1) + K - Left'First (2),
                                      Right'First (2) + J - Result'First (2));
            end loop;
         end loop;
      end loop;
      return Result;
   end "*";

   function "*" (Left : Real_Matrix; Right : Real_Vector) return Real_Vector is
      Result : Real_Vector (Left'Range (1));
   begin
      if Left'Length (2) /= Right'Length then
         raise Constraint_Error;
      end if;
      for I in Result'Range loop
         Result (I) := 0.0;
         for J in Left'Range (2) loop
            Result (I) := Result (I) +
              Left (I, J) * Right (Right'First + J - Left'First (2));
         end loop;
      end loop;
      return Result;
   end "*";

   function "*" (Left : Real_Vector; Right : Real_Matrix) return Real_Vector is
      Result : Real_Vector (Right'Range (2));
   begin
      if Left'Length /= Right'Length (1) then
         raise Constraint_Error;
      end if;
      for J in Result'Range loop
         Result (J) := 0.0;
         for I in Right'Range (1) loop
            Result (J) := Result (J) +
              Left (Left'First + I - Right'First (1)) * Right (I, J);
         end loop;
      end loop;
      return Result;
   end "*";

   function "*" (Left : Real'Base; Right : Real_Matrix) return Real_Matrix is
      Result : Real_Matrix (Right'Range (1), Right'Range (2));
   begin
      for I in Right'Range (1) loop
         for J in Right'Range (2) loop
            Result (I, J) := Left * Right (I, J);
         end loop;
      end loop;
      return Result;
   end "*";

   function "*" (Left : Real_Matrix; Right : Real'Base) return Real_Matrix is
      Result : Real_Matrix (Left'Range (1), Left'Range (2));
   begin
      for I in Left'Range (1) loop
         for J in Left'Range (2) loop
            Result (I, J) := Left (I, J) * Right;
         end loop;
      end loop;
      return Result;
   end "*";

   function "/" (Left : Real_Matrix; Right : Real'Base) return Real_Matrix is
      Result : Real_Matrix (Left'Range (1), Left'Range (2));
   begin
      for I in Left'Range (1) loop
         for J in Left'Range (2) loop
            Result (I, J) := Left (I, J) / Right;
         end loop;
      end loop;
      return Result;
   end "/";

   ---------------
   -- Transpose --
   ---------------

   function Transpose (X : Real_Matrix) return Real_Matrix is
      Result : Real_Matrix (X'Range (2), X'Range (1));
   begin
      for I in X'Range (1) loop
         for J in X'Range (2) loop
            Result (J, I) := X (I, J);
         end loop;
      end loop;
      return Result;
   end Transpose;

   -----------
   -- Solve --
   -----------

   function Solve (A : Real_Matrix; X : Real_Vector) return Real_Vector is
      -- Simplified Gaussian elimination for Z80
      N : constant Integer := A'Length (1);
      M : Real_Matrix (1 .. N, 1 .. N);
      B : Real_Vector (1 .. N);
      Result : Real_Vector (X'Range);
      Factor : Real'Base;
      Max_Row : Integer;
      Temp : Real'Base;
   begin
      if A'Length (1) /= A'Length (2) or A'Length (1) /= X'Length then
         raise Constraint_Error;
      end if;

      -- Copy to working arrays
      for I in 1 .. N loop
         B (I) := X (X'First + I - 1);
         for J in 1 .. N loop
            M (I, J) := A (A'First (1) + I - 1, A'First (2) + J - 1);
         end loop;
      end loop;

      -- Gaussian elimination with partial pivoting
      for K in 1 .. N - 1 loop
         Max_Row := K;
         for I in K + 1 .. N loop
            if abs M (I, K) > abs M (Max_Row, K) then
               Max_Row := I;
            end if;
         end loop;

         -- Swap rows
         if Max_Row /= K then
            for J in K .. N loop
               Temp := M (K, J);
               M (K, J) := M (Max_Row, J);
               M (Max_Row, J) := Temp;
            end loop;
            Temp := B (K);
            B (K) := B (Max_Row);
            B (Max_Row) := Temp;
         end if;

         -- Eliminate
         for I in K + 1 .. N loop
            if M (K, K) /= 0.0 then
               Factor := M (I, K) / M (K, K);
               for J in K .. N loop
                  M (I, J) := M (I, J) - Factor * M (K, J);
               end loop;
               B (I) := B (I) - Factor * B (K);
            end if;
         end loop;
      end loop;

      -- Back substitution
      for I in reverse 1 .. N loop
         Result (X'First + I - 1) := B (I);
         for J in I + 1 .. N loop
            Result (X'First + I - 1) := Result (X'First + I - 1) -
              M (I, J) * Result (X'First + J - 1);
         end loop;
         if M (I, I) /= 0.0 then
            Result (X'First + I - 1) := Result (X'First + I - 1) / M (I, I);
         end if;
      end loop;

      return Result;
   end Solve;

   function Solve (A, X : Real_Matrix) return Real_Matrix is
      Result : Real_Matrix (X'Range (1), X'Range (2));
      Col : Real_Vector (X'Range (1));
      Sol : Real_Vector (X'Range (1));
   begin
      for J in X'Range (2) loop
         for I in X'Range (1) loop
            Col (I) := X (I, J);
         end loop;
         Sol := Solve (A, Col);
         for I in X'Range (1) loop
            Result (I, J) := Sol (I);
         end loop;
      end loop;
      return Result;
   end Solve;

   -----------------
   -- Unit_Vector --
   -----------------

   function Unit_Vector
     (Index : Integer;
      Order : Positive;
      First : Integer := 1) return Real_Vector
   is
      Result : Real_Vector (First .. First + Order - 1) := (others => 0.0);
   begin
      if Index < First or Index >= First + Order then
         raise Constraint_Error;
      end if;
      Result (Index) := 1.0;
      return Result;
   end Unit_Vector;

   -----------------
   -- Unit_Matrix --
   -----------------

   function Unit_Matrix
     (Order   : Positive;
      First_1 : Integer := 1;
      First_2 : Integer := 1) return Real_Matrix
   is
      Result : Real_Matrix (First_1 .. First_1 + Order - 1,
                            First_2 .. First_2 + Order - 1) := (others => (others => 0.0));
   begin
      for I in 0 .. Order - 1 loop
         Result (First_1 + I, First_2 + I) := 1.0;
      end loop;
      return Result;
   end Unit_Matrix;

end Ada.Numerics.Generic_Real_Arrays;
