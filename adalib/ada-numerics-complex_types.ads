-- Ada.Numerics.Complex_Types for Z80
-- Concrete complex number types based on Float

with Ada.Numerics.Generic_Complex_Types;

package Ada.Numerics.Complex_Types is
  new Ada.Numerics.Generic_Complex_Types (Float);

pragma Pure (Ada.Numerics.Complex_Types);
