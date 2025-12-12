-- GNAT.Vector_Operations for Z80
-- Generic vector operations with fixed-point support

package GNAT.Vector_Operations is
   pragma Pure;

   Max_Size : constant := 16;  -- Maximum vector size for Z80

   -- Integer vector type
   type Int_Vector is array (1 .. Max_Size) of Integer;
   Zero_Int_Vector : constant Int_Vector := (others => 0);

   -- Fixed-point (8.8 format) vector type for Z80
   type Fixed_8_8 is delta 1.0 / 256.0 range -128.0 .. 127.996;
   type Fixed_Vector is array (1 .. Max_Size) of Fixed_8_8;
   Zero_Fixed_Vector : constant Fixed_Vector := (others => 0.0);

   -- Integer vector operations
   function Add (A, B : Int_Vector; Size : Positive) return Int_Vector;
   function Subtract (A, B : Int_Vector; Size : Positive) return Int_Vector;
   function Scale (A : Int_Vector; S : Integer; Size : Positive) return Int_Vector;
   function Dot (A, B : Int_Vector; Size : Positive) return Integer;
   function Norm_Squared (A : Int_Vector; Size : Positive) return Natural;
   function Min (A : Int_Vector; Size : Positive) return Integer;
   function Max (A : Int_Vector; Size : Positive) return Integer;
   function Sum (A : Int_Vector; Size : Positive) return Integer;
   function Average (A : Int_Vector; Size : Positive) return Integer;

   -- Fixed-point vector operations
   function Add (A, B : Fixed_Vector; Size : Positive) return Fixed_Vector;
   function Subtract (A, B : Fixed_Vector; Size : Positive) return Fixed_Vector;
   function Scale (A : Fixed_Vector; S : Fixed_8_8; Size : Positive) return Fixed_Vector;
   function Dot (A, B : Fixed_Vector; Size : Positive) return Fixed_8_8;
   function Norm_Squared (A : Fixed_Vector; Size : Positive) return Fixed_8_8;
   function Min (A : Fixed_Vector; Size : Positive) return Fixed_8_8;
   function Max (A : Fixed_Vector; Size : Positive) return Fixed_8_8;
   function Sum (A : Fixed_Vector; Size : Positive) return Fixed_8_8;
   function Average (A : Fixed_Vector; Size : Positive) return Fixed_8_8;

   -- Cross product for 3D vectors
   function Cross_3D (A, B : Int_Vector) return Int_Vector;
   function Cross_3D (A, B : Fixed_Vector) return Fixed_Vector;

   -- Utility operations
   procedure Fill (A : out Int_Vector; Value : Integer; Size : Positive);
   procedure Fill (A : out Fixed_Vector; Value : Fixed_8_8; Size : Positive);

   procedure Copy (Source : Int_Vector; Target : out Int_Vector; Size : Positive);
   procedure Copy (Source : Fixed_Vector; Target : out Fixed_Vector; Size : Positive);

   function Equal (A, B : Int_Vector; Size : Positive) return Boolean;
   function Equal (A, B : Fixed_Vector; Size : Positive) return Boolean;

end GNAT.Vector_Operations;
