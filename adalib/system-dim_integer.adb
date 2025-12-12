-- System.Dim_Integer body for Z80
-- Dimension system for integer types

package body System.Dim_Integer is

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

end System.Dim_Integer;
