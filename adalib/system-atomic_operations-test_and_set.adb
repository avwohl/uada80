-- System.Atomic_Operations.Test_And_Set body for Z80
-- Atomic test-and-set flag operations implementation

package body System.Atomic_Operations.Test_And_Set is

   -------------------------
   -- Atomic_Test_And_Set --
   -------------------------

   function Atomic_Test_And_Set
     (Item : aliased in Out Test_And_Set_Flag) return Boolean
   is
      Old_Value : constant Boolean := Boolean (Item);
   begin
      -- On Z80, this would use DI/EI for interrupt safety
      Item := Test_And_Set_Flag (True);
      return Old_Value;
   end Atomic_Test_And_Set;

   ------------------
   -- Atomic_Clear --
   ------------------

   procedure Atomic_Clear
     (Item : aliased in Out Test_And_Set_Flag)
   is
   begin
      Item := Test_And_Set_Flag (False);
   end Atomic_Clear;

   ------------------
   -- Is_Lock_Free --
   ------------------

   function Is_Lock_Free return Boolean is
   begin
      return True;
   end Is_Lock_Free;

end System.Atomic_Operations.Test_And_Set;
