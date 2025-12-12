-- Ada.Finalization body for Z80
-- Finalization runtime support

package body Ada.Finalization is

   -- The actual finalization is handled by compiler-generated code
   -- and the runtime library. The procedures here provide default
   -- null implementations that can be overridden.

   -- Note: The compiler will generate calls to Initialize, Adjust,
   -- and Finalize at appropriate points based on type derivation
   -- from Controlled or Limited_Controlled.

end Ada.Finalization;
