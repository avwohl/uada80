-- GNAT.Vector_Operations body for Z80
-- Generic vector operations implementation

package body GNAT.Vector_Operations is

   -- Integer vector operations

   function Add (A, B : Int_Vector; Size : Positive) return Int_Vector is
      Result : Int_Vector := Zero_Int_Vector;
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         Result (I) := A (I) + B (I);
      end loop;
      return Result;
   end Add;

   function Subtract (A, B : Int_Vector; Size : Positive) return Int_Vector is
      Result : Int_Vector := Zero_Int_Vector;
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         Result (I) := A (I) - B (I);
      end loop;
      return Result;
   end Subtract;

   function Scale (A : Int_Vector; S : Integer; Size : Positive) return Int_Vector is
      Result : Int_Vector := Zero_Int_Vector;
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         Result (I) := A (I) * S;
      end loop;
      return Result;
   end Scale;

   function Dot (A, B : Int_Vector; Size : Positive) return Integer is
      Result : Integer := 0;
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         Result := Result + A (I) * B (I);
      end loop;
      return Result;
   end Dot;

   function Norm_Squared (A : Int_Vector; Size : Positive) return Natural is
   begin
      return Natural (Dot (A, A, Size));
   end Norm_Squared;

   function Min (A : Int_Vector; Size : Positive) return Integer is
      Result : Integer := Integer'Last;
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         if A (I) < Result then
            Result := A (I);
         end if;
      end loop;
      return Result;
   end Min;

   function Max (A : Int_Vector; Size : Positive) return Integer is
      Result : Integer := Integer'First;
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         if A (I) > Result then
            Result := A (I);
         end if;
      end loop;
      return Result;
   end Max;

   function Sum (A : Int_Vector; Size : Positive) return Integer is
      Result : Integer := 0;
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         Result := Result + A (I);
      end loop;
      return Result;
   end Sum;

   function Average (A : Int_Vector; Size : Positive) return Integer is
      S : constant Positive := Positive'Min (Size, Max_Size);
   begin
      return Sum (A, S) / S;
   end Average;

   -- Fixed-point vector operations

   function Add (A, B : Fixed_Vector; Size : Positive) return Fixed_Vector is
      Result : Fixed_Vector := Zero_Fixed_Vector;
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         Result (I) := A (I) + B (I);
      end loop;
      return Result;
   end Add;

   function Subtract (A, B : Fixed_Vector; Size : Positive) return Fixed_Vector is
      Result : Fixed_Vector := Zero_Fixed_Vector;
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         Result (I) := A (I) - B (I);
      end loop;
      return Result;
   end Subtract;

   function Scale (A : Fixed_Vector; S : Fixed_8_8; Size : Positive) return Fixed_Vector is
      Result : Fixed_Vector := Zero_Fixed_Vector;
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         Result (I) := A (I) * S;
      end loop;
      return Result;
   end Scale;

   function Dot (A, B : Fixed_Vector; Size : Positive) return Fixed_8_8 is
      Result : Fixed_8_8 := 0.0;
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         Result := Result + A (I) * B (I);
      end loop;
      return Result;
   end Dot;

   function Norm_Squared (A : Fixed_Vector; Size : Positive) return Fixed_8_8 is
   begin
      return Dot (A, A, Size);
   end Norm_Squared;

   function Min (A : Fixed_Vector; Size : Positive) return Fixed_8_8 is
      Result : Fixed_8_8 := Fixed_8_8'Last;
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         if A (I) < Result then
            Result := A (I);
         end if;
      end loop;
      return Result;
   end Min;

   function Max (A : Fixed_Vector; Size : Positive) return Fixed_8_8 is
      Result : Fixed_8_8 := Fixed_8_8'First;
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         if A (I) > Result then
            Result := A (I);
         end if;
      end loop;
      return Result;
   end Max;

   function Sum (A : Fixed_Vector; Size : Positive) return Fixed_8_8 is
      Result : Fixed_8_8 := 0.0;
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         Result := Result + A (I);
      end loop;
      return Result;
   end Sum;

   function Average (A : Fixed_Vector; Size : Positive) return Fixed_8_8 is
      S : constant Positive := Positive'Min (Size, Max_Size);
   begin
      return Sum (A, S) / Fixed_8_8 (S);
   end Average;

   -- Cross product for 3D vectors

   function Cross_3D (A, B : Int_Vector) return Int_Vector is
      Result : Int_Vector := Zero_Int_Vector;
   begin
      Result (1) := A (2) * B (3) - A (3) * B (2);
      Result (2) := A (3) * B (1) - A (1) * B (3);
      Result (3) := A (1) * B (2) - A (2) * B (1);
      return Result;
   end Cross_3D;

   function Cross_3D (A, B : Fixed_Vector) return Fixed_Vector is
      Result : Fixed_Vector := Zero_Fixed_Vector;
   begin
      Result (1) := A (2) * B (3) - A (3) * B (2);
      Result (2) := A (3) * B (1) - A (1) * B (3);
      Result (3) := A (1) * B (2) - A (2) * B (1);
      return Result;
   end Cross_3D;

   -- Utility operations

   procedure Fill (A : out Int_Vector; Value : Integer; Size : Positive) is
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         A (I) := Value;
      end loop;
   end Fill;

   procedure Fill (A : out Fixed_Vector; Value : Fixed_8_8; Size : Positive) is
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         A (I) := Value;
      end loop;
   end Fill;

   procedure Copy (Source : Int_Vector; Target : out Int_Vector; Size : Positive) is
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         Target (I) := Source (I);
      end loop;
   end Copy;

   procedure Copy (Source : Fixed_Vector; Target : out Fixed_Vector; Size : Positive) is
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         Target (I) := Source (I);
      end loop;
   end Copy;

   function Equal (A, B : Int_Vector; Size : Positive) return Boolean is
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         if A (I) /= B (I) then
            return False;
         end if;
      end loop;
      return True;
   end Equal;

   function Equal (A, B : Fixed_Vector; Size : Positive) return Boolean is
   begin
      for I in 1 .. Positive'Min (Size, Max_Size) loop
         if A (I) /= B (I) then
            return False;
         end if;
      end loop;
      return True;
   end Equal;

end GNAT.Vector_Operations;
