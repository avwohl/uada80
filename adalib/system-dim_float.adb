-- System.Dim_Float body for Z80
-- Dimension system for floating-point types

package body System.Dim_Float is

   ---------
   -- "+" --
   ---------

   function "+" (L, R : Dimension_Vector) return Dimension_Vector is
      Result : Dimension_Vector;
   begin
      for I in Dimension_Vector'Range loop
         Result (I) := L (I) + R (I);
      end loop;
      return Result;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L, R : Dimension_Vector) return Dimension_Vector is
      Result : Dimension_Vector;
   begin
      for I in Dimension_Vector'Range loop
         Result (I) := L (I) - R (I);
      end loop;
      return Result;
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (L : Dimension_Vector; R : Integer) return Dimension_Vector is
      Result : Dimension_Vector;
   begin
      for I in Dimension_Vector'Range loop
         Result (I) := L (I) * Dimension (R);
      end loop;
      return Result;
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (L : Dimension_Vector; R : Integer) return Dimension_Vector is
      Result : Dimension_Vector;
   begin
      for I in Dimension_Vector'Range loop
         Result (I) := L (I) / Dimension (R);
      end loop;
      return Result;
   end "/";

   --------------------
   -- Same_Dimension --
   --------------------

   function Same_Dimension (L, R : Dimension_Vector) return Boolean is
   begin
      for I in Dimension_Vector'Range loop
         if L (I) /= R (I) then
            return False;
         end if;
      end loop;
      return True;
   end Same_Dimension;

end System.Dim_Float;
