-- System.Dim_Integer for Z80
-- Dimension system for integer types

package System.Dim_Integer is
   pragma Pure;

   -- Base dimension type
   type Dimension is range -127 .. 127;

   -- Common dimensions
   Dimensionless : constant Dimension := 0;

   -- Dimension vector (simplified for Z80)
   type Dimension_Vector is array (1 .. 7) of Dimension;

   -- Null dimension vector
   Null_Dimension : constant Dimension_Vector := (others => 0);

   -- Basic dimension indices
   Length_Dim      : constant := 1;  -- meters
   Mass_Dim        : constant := 2;  -- kilograms
   Time_Dim        : constant := 3;  -- seconds
   Current_Dim     : constant := 4;  -- amperes
   Temperature_Dim : constant := 5;  -- kelvin
   Amount_Dim      : constant := 6;  -- moles
   Luminosity_Dim  : constant := 7;  -- candelas

   -- Dimension arithmetic
   function "+" (L, R : Dimension_Vector) return Dimension_Vector;
   function "-" (L, R : Dimension_Vector) return Dimension_Vector;
   function "*" (L : Dimension_Vector; R : Integer) return Dimension_Vector;

end System.Dim_Integer;
