-- Ada.Numerics.Complex_Arrays for Z80
-- Complex array operations for default Float type

with Ada.Numerics.Real_Arrays;
with Ada.Numerics.Complex_Types;
with Ada.Numerics.Generic_Complex_Arrays;

package Ada.Numerics.Complex_Arrays is new
   Ada.Numerics.Generic_Complex_Arrays
     (Real_Arrays   => Ada.Numerics.Real_Arrays,
      Complex_Types => Ada.Numerics.Complex_Types);
pragma Pure (Ada.Numerics.Complex_Arrays);
