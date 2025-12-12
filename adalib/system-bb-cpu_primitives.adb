-- System.BB.CPU_Primitives body for Z80
-- Low-level CPU operations implementation

package body System.BB.CPU_Primitives is

   ------------------------
   -- Initialize_Context --
   ------------------------

   procedure Initialize_Context
     (Buffer     : out Context_Buffer;
      Entry_Point : System.Address;
      Stack_Top   : System.Address;
      Arg         : System.Address)
   is
      use System.Storage_Elements;
   begin
      -- Initialize all registers to zero
      Buffer := (AF  => 0, BC => 0, DE => 0, HL => 0,
                 AF_Alt => 0, BC_Alt => 0, DE_Alt => 0, HL_Alt => 0,
                 IX  => 0, IY => 0,
                 SP  => Natural (To_Integer (Stack_Top)),
                 PC  => Natural (To_Integer (Entry_Point)),
                 I   => 0, R => 0,
                 IFF1 => True, IFF2 => True);

      -- Pass argument in HL
      Buffer.HL := Natural (To_Integer (Arg));
   end Initialize_Context;

   --------------------
   -- Context_Switch --
   --------------------

   procedure Context_Switch
     (Running_Context : in Out Context_Buffer;
      First_Context   : in Context_Buffer)
   is
      pragma Unreferenced (Running_Context, First_Context);
   begin
      -- This would be implemented in assembly:
      -- 1. Save all registers of running task
      -- 2. Save SP to Running_Context
      -- 3. Load SP from First_Context
      -- 4. Restore all registers of new task
      -- 5. RET (or JP to PC)
      null;
   end Context_Switch;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts is
   begin
      -- DI instruction
      null;  -- Would be: pragma Machine_Code (Asm ("DI"));
   end Disable_Interrupts;
   pragma Export (C, Disable_Interrupts, "_di");

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts is
   begin
      -- EI instruction
      null;  -- Would be: pragma Machine_Code (Asm ("EI"));
   end Enable_Interrupts;
   pragma Export (C, Enable_Interrupts, "_ei");

   -------------------------
   -- Get_Interrupt_State --
   -------------------------

   function Get_Interrupt_State return Boolean is
   begin
      -- Read IFF state (not directly accessible on Z80)
      -- Would need to track in software
      return True;
   end Get_Interrupt_State;

   -------------------------
   -- Set_Interrupt_State --
   -------------------------

   procedure Set_Interrupt_State (State : Boolean) is
   begin
      if State then
         Enable_Interrupts;
      else
         Disable_Interrupts;
      end if;
   end Set_Interrupt_State;

   ------------
   -- Get_SP --
   ------------

   function Get_SP return System.Address is
   begin
      -- LD HL, 0 / ADD HL, SP
      return System.Null_Address;  -- Stub
   end Get_SP;

   ---------
   -- NOP --
   ---------

   procedure NOP is
   begin
      -- NOP instruction (4 T-states)
      null;
   end NOP;

   ----------
   -- Halt --
   ----------

   procedure Halt is
   begin
      -- HALT instruction
      null;
   end Halt;

end System.BB.CPU_Primitives;
