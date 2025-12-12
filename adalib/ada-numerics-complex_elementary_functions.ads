-- Ada.Numerics.Complex_Elementary_Functions for Z80
-- Concrete complex elementary functions based on Float

with Ada.Numerics.Complex_Types;
with Ada.Numerics.Generic_Complex_Elementary_Functions;

package Ada.Numerics.Complex_Elementary_Functions is
  new Ada.Numerics.Generic_Complex_Elementary_Functions
    (Ada.Numerics.Complex_Types);

pragma Pure (Ada.Numerics.Complex_Elementary_Functions);
