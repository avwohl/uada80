-- System.Atomic_Operations.Test_And_Set for Z80
-- Atomic test-and-set flag operations

package System.Atomic_Operations.Test_And_Set
  with Pure
is
   type Test_And_Set_Flag is private;
   pragma Preelaborable_Initialization (Test_And_Set_Flag);

   function Atomic_Test_And_Set
     (Item : aliased in Out Test_And_Set_Flag) return Boolean
     with Convention => Intrinsic;

   procedure Atomic_Clear
     (Item : aliased in Out Test_And_Set_Flag)
     with Convention => Intrinsic;

   function Is_Lock_Free return Boolean
     with Convention => Intrinsic;

private
   type Test_And_Set_Flag is new Boolean;

end System.Atomic_Operations.Test_And_Set;
