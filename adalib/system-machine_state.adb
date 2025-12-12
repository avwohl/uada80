-- System.Machine_State body for Z80
-- CPU register state for exception handling

package body System.Machine_State is

   ----------------
   -- Save_State --
   ----------------

   procedure Save_State (State : out Register_Set) is
   begin
      -- This would normally be implemented in assembly
      -- For now, provide stub implementation
      State := Null_State;
   end Save_State;

   -------------------
   -- Restore_State --
   -------------------

   procedure Restore_State (State : Register_Set) is
      pragma Unreferenced (State);
   begin
      -- This would normally be implemented in assembly
      -- and would not return
      null;
   end Restore_State;

   ------------
   -- Get_PC --
   ------------

   function Get_PC (State : Register_Set) return System.Address is
   begin
      return System.Address (State.PC);
   end Get_PC;

   ------------
   -- Get_SP --
   ------------

   function Get_SP (State : Register_Set) return System.Address is
   begin
      return System.Address (State.SP);
   end Get_SP;

end System.Machine_State;
