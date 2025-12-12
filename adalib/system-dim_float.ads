-- System.Dim_Float for Z80
-- Dimension system for floating-point types

package System.Dim_Float is
   pragma Pure;

   -- Dimension exponent type
   type Dimension is range -127 .. 127;

   -- Dimension vector for SI units
   type Dimension_Vector is array (1 .. 7) of Dimension;

   -- Null dimensions
   Null_Dimension : constant Dimension_Vector := (others => 0);

   -- SI base unit dimension indices
   Meter_Dim    : constant := 1;  -- Length
   Kilogram_Dim : constant := 2;  -- Mass
   Second_Dim   : constant := 3;  -- Time
   Ampere_Dim   : constant := 4;  -- Electric current
   Kelvin_Dim   : constant := 5;  -- Temperature
   Mole_Dim     : constant := 6;  -- Amount
   Candela_Dim  : constant := 7;  -- Luminous intensity

   -- Common derived dimension vectors
   Velocity_Dim     : constant Dimension_Vector := (1, 0, -1, 0, 0, 0, 0);
   Acceleration_Dim : constant Dimension_Vector := (1, 0, -2, 0, 0, 0, 0);
   Force_Dim        : constant Dimension_Vector := (1, 1, -2, 0, 0, 0, 0);
   Energy_Dim       : constant Dimension_Vector := (2, 1, -2, 0, 0, 0, 0);
   Power_Dim        : constant Dimension_Vector := (2, 1, -3, 0, 0, 0, 0);

   -- Dimension arithmetic
   function "+" (L, R : Dimension_Vector) return Dimension_Vector;
   function "-" (L, R : Dimension_Vector) return Dimension_Vector;
   function "*" (L : Dimension_Vector; R : Integer) return Dimension_Vector;
   function "/" (L : Dimension_Vector; R : Integer) return Dimension_Vector;

   -- Check dimensions match
   function Same_Dimension (L, R : Dimension_Vector) return Boolean;

end System.Dim_Float;
