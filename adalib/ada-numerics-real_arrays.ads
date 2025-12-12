-- Ada.Numerics.Real_Arrays for Z80
-- Real vector and matrix operations (instantiation)

with Ada.Numerics.Generic_Real_Arrays;

package Ada.Numerics.Real_Arrays is
   new Ada.Numerics.Generic_Real_Arrays (Float);

pragma Preelaborate (Ada.Numerics.Real_Arrays);
