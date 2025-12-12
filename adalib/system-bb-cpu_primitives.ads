-- System.BB.CPU_Primitives for Z80
-- Low-level CPU operations

package System.BB.CPU_Primitives is
   pragma Preelaborate;

   -- Z80 register context for task switching
   type Context_Buffer is record
      AF  : Natural;
      BC  : Natural;
      DE  : Natural;
      HL  : Natural;
      AF_Alt : Natural;
      BC_Alt : Natural;
      DE_Alt : Natural;
      HL_Alt : Natural;
      IX  : Natural;
      IY  : Natural;
      SP  : Natural;
      PC  : Natural;
      I   : Natural;  -- Interrupt vector register
      R   : Natural;  -- Refresh register
      IFF1 : Boolean; -- Interrupt flip-flop 1
      IFF2 : Boolean; -- Interrupt flip-flop 2
   end record;

   -- Initialize context for a new task
   procedure Initialize_Context
     (Buffer     : out Context_Buffer;
      Entry_Point : System.Address;
      Stack_Top   : System.Address;
      Arg         : System.Address);

   -- Context switch
   procedure Context_Switch
     (Running_Context : in Out Context_Buffer;
      First_Context   : in Context_Buffer);

   -- Disable/Enable interrupts
   procedure Disable_Interrupts;
   procedure Enable_Interrupts;

   -- Save/Restore interrupt state
   function Get_Interrupt_State return Boolean;
   procedure Set_Interrupt_State (State : Boolean);

   -- Get current stack pointer
   function Get_SP return System.Address;

   -- No-op for timing
   procedure NOP;

   -- Halt CPU
   procedure Halt;

end System.BB.CPU_Primitives;
