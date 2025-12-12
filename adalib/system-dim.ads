-- System.Dim for Z80
-- Dimension system support for physical units

package System.Dim is
   pragma Pure;

   -- Dimension checking is done at compile time through aspect specifications
   -- This package provides the foundation types

   -- Base dimension representation
   -- Dimensions are represented as exponents of base units:
   -- Length, Mass, Time, Electric_Current, Temperature, Amount, Luminosity

   type Dimension_Exponent is range -7 .. 7;
   for Dimension_Exponent'Size use 4;

   type Dimension_Type is record
      Length            : Dimension_Exponent := 0;  -- meters
      Mass              : Dimension_Exponent := 0;  -- kilograms
      Time              : Dimension_Exponent := 0;  -- seconds
      Electric_Current  : Dimension_Exponent := 0;  -- amperes
      Temperature       : Dimension_Exponent := 0;  -- kelvin
      Amount_Of_Substance : Dimension_Exponent := 0; -- moles
      Luminous_Intensity : Dimension_Exponent := 0; -- candelas
   end record;
   pragma Pack (Dimension_Type);

   -- Predefined base dimensions
   Dimensionless : constant Dimension_Type := (others => 0);

   Length_Dimension : constant Dimension_Type :=
     (Length => 1, others => 0);

   Mass_Dimension : constant Dimension_Type :=
     (Mass => 1, others => 0);

   Time_Dimension : constant Dimension_Type :=
     (Time => 1, others => 0);

   Current_Dimension : constant Dimension_Type :=
     (Electric_Current => 1, others => 0);

   Temperature_Dimension : constant Dimension_Type :=
     (Temperature => 1, others => 0);

   -- Derived dimensions
   Velocity_Dimension : constant Dimension_Type :=
     (Length => 1, Time => -1, others => 0);

   Acceleration_Dimension : constant Dimension_Type :=
     (Length => 1, Time => -2, others => 0);

   Force_Dimension : constant Dimension_Type :=
     (Length => 1, Mass => 1, Time => -2, others => 0);

   Energy_Dimension : constant Dimension_Type :=
     (Length => 2, Mass => 1, Time => -2, others => 0);

   Power_Dimension : constant Dimension_Type :=
     (Length => 2, Mass => 1, Time => -3, others => 0);

   -- Dimension arithmetic
   function Multiply (Left, Right : Dimension_Type) return Dimension_Type;
   function Divide (Left, Right : Dimension_Type) return Dimension_Type;
   function Power (D : Dimension_Type; N : Integer) return Dimension_Type;
   function Root (D : Dimension_Type; N : Positive) return Dimension_Type;

   function "=" (Left, Right : Dimension_Type) return Boolean;

end System.Dim;
