-- Ada.Numerics.Generic_Complex_Arrays body for Z80
-- Generic complex array operations implementation

package body Ada.Numerics.Generic_Complex_Arrays is

   ---------
   -- "+" --
   ---------

   function "+" (Right : Complex_Vector) return Complex_Vector is
   begin
      return Right;
   end "+";

   function "-" (Right : Complex_Vector) return Complex_Vector is
      Result : Complex_Vector (Right'Range);
   begin
      for I in Right'Range loop
         Result (I) := -Right (I);
      end loop;
      return Result;
   end "-";

   function Conjugate (X : Complex_Vector) return Complex_Vector is
      Result : Complex_Vector (X'Range);
   begin
      for I in X'Range loop
         Result (I) := Conjugate (X (I));
      end loop;
      return Result;
   end Conjugate;

   function "+" (Left, Right : Complex_Vector) return Complex_Vector is
      Result : Complex_Vector (Left'Range);
   begin
      for I in Left'Range loop
         Result (I) := Left (I) + Right (I - Left'First + Right'First);
      end loop;
      return Result;
   end "+";

   function "-" (Left, Right : Complex_Vector) return Complex_Vector is
      Result : Complex_Vector (Left'Range);
   begin
      for I in Left'Range loop
         Result (I) := Left (I) - Right (I - Left'First + Right'First);
      end loop;
      return Result;
   end "-";

   function "*" (Left : Complex; Right : Complex_Vector) return Complex_Vector is
      Result : Complex_Vector (Right'Range);
   begin
      for I in Right'Range loop
         Result (I) := Left * Right (I);
      end loop;
      return Result;
   end "*";

   function "*" (Left : Complex_Vector; Right : Complex) return Complex_Vector is
      Result : Complex_Vector (Left'Range);
   begin
      for I in Left'Range loop
         Result (I) := Left (I) * Right;
      end loop;
      return Result;
   end "*";

   function "/" (Left : Complex_Vector; Right : Complex) return Complex_Vector is
      Result : Complex_Vector (Left'Range);
   begin
      for I in Left'Range loop
         Result (I) := Left (I) / Right;
      end loop;
      return Result;
   end "/";

   function "*" (Left : Real'Base; Right : Complex_Vector) return Complex_Vector is
   begin
      return Compose_From_Cartesian (Left, 0.0) * Right;
   end "*";

   function "*" (Left : Complex_Vector; Right : Real'Base) return Complex_Vector is
   begin
      return Left * Compose_From_Cartesian (Right, 0.0);
   end "*";

   function "/" (Left : Complex_Vector; Right : Real'Base) return Complex_Vector is
   begin
      return Left / Compose_From_Cartesian (Right, 0.0);
   end "/";

   -- Inner product
   function "*" (Left, Right : Complex_Vector) return Complex is
      Result : Complex := (Re => 0.0, Im => 0.0);
   begin
      for I in Left'Range loop
         Result := Result + Conjugate (Left (I)) * Right (I - Left'First + Right'First);
      end loop;
      return Result;
   end "*";

   function "abs" (Right : Complex_Vector) return Real_Vector is
      Result : Real_Vector (Right'Range);
   begin
      for I in Right'Range loop
         Result (I) := abs (Right (I));
      end loop;
      return Result;
   end "abs";

   function Unit_Vector
     (Index : Integer;
      Order : Positive;
      First : Integer := 1) return Complex_Vector
   is
      Result : Complex_Vector (First .. First + Order - 1) :=
        (others => (Re => 0.0, Im => 0.0));
   begin
      Result (Index) := (Re => 1.0, Im => 0.0);
      return Result;
   end Unit_Vector;

   -- Matrix operations

   function "+" (Right : Complex_Matrix) return Complex_Matrix is
   begin
      return Right;
   end "+";

   function "-" (Right : Complex_Matrix) return Complex_Matrix is
      Result : Complex_Matrix (Right'Range (1), Right'Range (2));
   begin
      for I in Right'Range (1) loop
         for J in Right'Range (2) loop
            Result (I, J) := -Right (I, J);
         end loop;
      end loop;
      return Result;
   end "-";

   function Conjugate (X : Complex_Matrix) return Complex_Matrix is
      Result : Complex_Matrix (X'Range (1), X'Range (2));
   begin
      for I in X'Range (1) loop
         for J in X'Range (2) loop
            Result (I, J) := Conjugate (X (I, J));
         end loop;
      end loop;
      return Result;
   end Conjugate;

   function Transpose (X : Complex_Matrix) return Complex_Matrix is
      Result : Complex_Matrix (X'Range (2), X'Range (1));
   begin
      for I in X'Range (1) loop
         for J in X'Range (2) loop
            Result (J, I) := X (I, J);
         end loop;
      end loop;
      return Result;
   end Transpose;

   function "+" (Left, Right : Complex_Matrix) return Complex_Matrix is
      Result : Complex_Matrix (Left'Range (1), Left'Range (2));
   begin
      for I in Left'Range (1) loop
         for J in Left'Range (2) loop
            Result (I, J) := Left (I, J) +
              Right (I - Left'First (1) + Right'First (1),
                     J - Left'First (2) + Right'First (2));
         end loop;
      end loop;
      return Result;
   end "+";

   function "-" (Left, Right : Complex_Matrix) return Complex_Matrix is
      Result : Complex_Matrix (Left'Range (1), Left'Range (2));
   begin
      for I in Left'Range (1) loop
         for J in Left'Range (2) loop
            Result (I, J) := Left (I, J) -
              Right (I - Left'First (1) + Right'First (1),
                     J - Left'First (2) + Right'First (2));
         end loop;
      end loop;
      return Result;
   end "-";

   function "*" (Left, Right : Complex_Matrix) return Complex_Matrix is
      Result : Complex_Matrix (Left'Range (1), Right'Range (2));
      Sum    : Complex;
   begin
      for I in Left'Range (1) loop
         for J in Right'Range (2) loop
            Sum := (Re => 0.0, Im => 0.0);
            for K in Left'Range (2) loop
               Sum := Sum + Left (I, K) *
                 Right (K - Left'First (2) + Right'First (1), J);
            end loop;
            Result (I, J) := Sum;
         end loop;
      end loop;
      return Result;
   end "*";

   function "*" (Left : Complex; Right : Complex_Matrix) return Complex_Matrix is
      Result : Complex_Matrix (Right'Range (1), Right'Range (2));
   begin
      for I in Right'Range (1) loop
         for J in Right'Range (2) loop
            Result (I, J) := Left * Right (I, J);
         end loop;
      end loop;
      return Result;
   end "*";

   function "*" (Left : Complex_Matrix; Right : Complex) return Complex_Matrix is
      Result : Complex_Matrix (Left'Range (1), Left'Range (2));
   begin
      for I in Left'Range (1) loop
         for J in Left'Range (2) loop
            Result (I, J) := Left (I, J) * Right;
         end loop;
      end loop;
      return Result;
   end "*";

   function "/" (Left : Complex_Matrix; Right : Complex) return Complex_Matrix is
      Result : Complex_Matrix (Left'Range (1), Left'Range (2));
   begin
      for I in Left'Range (1) loop
         for J in Left'Range (2) loop
            Result (I, J) := Left (I, J) / Right;
         end loop;
      end loop;
      return Result;
   end "/";

   function "*" (Left : Real'Base; Right : Complex_Matrix) return Complex_Matrix is
   begin
      return Compose_From_Cartesian (Left, 0.0) * Right;
   end "*";

   function "*" (Left : Complex_Matrix; Right : Real'Base) return Complex_Matrix is
   begin
      return Left * Compose_From_Cartesian (Right, 0.0);
   end "*";

   function "/" (Left : Complex_Matrix; Right : Real'Base) return Complex_Matrix is
   begin
      return Left / Compose_From_Cartesian (Right, 0.0);
   end "/";

   function "*" (Left : Complex_Matrix; Right : Complex_Vector) return Complex_Vector is
      Result : Complex_Vector (Left'Range (1));
      Sum    : Complex;
   begin
      for I in Left'Range (1) loop
         Sum := (Re => 0.0, Im => 0.0);
         for J in Left'Range (2) loop
            Sum := Sum + Left (I, J) * Right (J - Left'First (2) + Right'First);
         end loop;
         Result (I) := Sum;
      end loop;
      return Result;
   end "*";

   function "*" (Left : Complex_Vector; Right : Complex_Matrix) return Complex_Vector is
      Result : Complex_Vector (Right'Range (2));
      Sum    : Complex;
   begin
      for J in Right'Range (2) loop
         Sum := (Re => 0.0, Im => 0.0);
         for I in Right'Range (1) loop
            Sum := Sum + Left (I - Right'First (1) + Left'First) * Right (I, J);
         end loop;
         Result (J) := Sum;
      end loop;
      return Result;
   end "*";

   -- Outer product
   function "*" (Left, Right : Complex_Vector) return Complex_Matrix is
      Result : Complex_Matrix (Left'Range, Right'Range);
   begin
      for I in Left'Range loop
         for J in Right'Range loop
            Result (I, J) := Left (I) * Conjugate (Right (J));
         end loop;
      end loop;
      return Result;
   end "*";

   function Unit_Matrix
     (Order            : Positive;
      First_1, First_2 : Integer := 1) return Complex_Matrix
   is
      Result : Complex_Matrix (First_1 .. First_1 + Order - 1,
                               First_2 .. First_2 + Order - 1) :=
        (others => (others => (Re => 0.0, Im => 0.0)));
   begin
      for I in 0 .. Order - 1 loop
         Result (First_1 + I, First_2 + I) := (Re => 1.0, Im => 0.0);
      end loop;
      return Result;
   end Unit_Matrix;

   -- Conversions

   function Compose_From_Cartesian (Re : Real_Vector) return Complex_Vector is
      Result : Complex_Vector (Re'Range);
   begin
      for I in Re'Range loop
         Result (I) := (Re => Re (I), Im => 0.0);
      end loop;
      return Result;
   end Compose_From_Cartesian;

   function Compose_From_Cartesian (Re, Im : Real_Vector) return Complex_Vector is
      Result : Complex_Vector (Re'Range);
   begin
      for I in Re'Range loop
         Result (I) := (Re => Re (I), Im => Im (I - Re'First + Im'First));
      end loop;
      return Result;
   end Compose_From_Cartesian;

   function Compose_From_Cartesian (Re : Real_Matrix) return Complex_Matrix is
      Result : Complex_Matrix (Re'Range (1), Re'Range (2));
   begin
      for I in Re'Range (1) loop
         for J in Re'Range (2) loop
            Result (I, J) := (Re => Re (I, J), Im => 0.0);
         end loop;
      end loop;
      return Result;
   end Compose_From_Cartesian;

   function Compose_From_Cartesian (Re, Im : Real_Matrix) return Complex_Matrix is
      Result : Complex_Matrix (Re'Range (1), Re'Range (2));
   begin
      for I in Re'Range (1) loop
         for J in Re'Range (2) loop
            Result (I, J) := (Re => Re (I, J),
                              Im => Im (I - Re'First (1) + Im'First (1),
                                        J - Re'First (2) + Im'First (2)));
         end loop;
      end loop;
      return Result;
   end Compose_From_Cartesian;

   function Re (X : Complex_Vector) return Real_Vector is
      Result : Real_Vector (X'Range);
   begin
      for I in X'Range loop
         Result (I) := Re (X (I));
      end loop;
      return Result;
   end Re;

   function Im (X : Complex_Vector) return Real_Vector is
      Result : Real_Vector (X'Range);
   begin
      for I in X'Range loop
         Result (I) := Im (X (I));
      end loop;
      return Result;
   end Im;

   function Re (X : Complex_Matrix) return Real_Matrix is
      Result : Real_Matrix (X'Range (1), X'Range (2));
   begin
      for I in X'Range (1) loop
         for J in X'Range (2) loop
            Result (I, J) := Re (X (I, J));
         end loop;
      end loop;
      return Result;
   end Re;

   function Im (X : Complex_Matrix) return Real_Matrix is
      Result : Real_Matrix (X'Range (1), X'Range (2));
   begin
      for I in X'Range (1) loop
         for J in X'Range (2) loop
            Result (I, J) := Im (X (I, J));
         end loop;
      end loop;
      return Result;
   end Im;

   procedure Set_Re (X : in Out Complex_Vector; Re : Real_Vector) is
   begin
      for I in X'Range loop
         X (I).Re := Re (I - X'First + Re'First);
      end loop;
   end Set_Re;

   procedure Set_Im (X : in Out Complex_Vector; Im : Real_Vector) is
   begin
      for I in X'Range loop
         X (I).Im := Im (I - X'First + Im'First);
      end loop;
   end Set_Im;

   procedure Set_Re (X : in Out Complex_Matrix; Re : Real_Matrix) is
   begin
      for I in X'Range (1) loop
         for J in X'Range (2) loop
            X (I, J).Re := Re (I - X'First (1) + Re'First (1),
                               J - X'First (2) + Re'First (2));
         end loop;
      end loop;
   end Set_Re;

   procedure Set_Im (X : in Out Complex_Matrix; Im : Real_Matrix) is
   begin
      for I in X'Range (1) loop
         for J in X'Range (2) loop
            X (I, J).Im := Im (I - X'First (1) + Im'First (1),
                               J - X'First (2) + Im'First (2));
         end loop;
      end loop;
   end Set_Im;

end Ada.Numerics.Generic_Complex_Arrays;
