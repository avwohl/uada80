-- System.Machine_State for Z80
-- CPU register state for exception handling

package System.Machine_State is
   pragma Preelaborate;

   -- Z80 register set
   type Register_Set is record
      -- Main registers
      A  : Natural;
      F  : Natural;  -- Flags
      BC : Natural;
      DE : Natural;
      HL : Natural;
      -- Index registers
      IX : Natural;
      IY : Natural;
      -- Stack and program counter
      SP : Natural;
      PC : Natural;
   end record;

   -- Null state
   Null_State : constant Register_Set := (others => 0);

   -- Save current machine state
   procedure Save_State (State : out Register_Set);

   -- Restore machine state
   procedure Restore_State (State : Register_Set);

   -- Get program counter from state
   function Get_PC (State : Register_Set) return System.Address;

   -- Get stack pointer from state
   function Get_SP (State : Register_Set) return System.Address;

end System.Machine_State;
