-- System.Interrupt_Handlers body for Z80
-- Interrupt handler support

package body System.Interrupt_Handlers is

   -- Interrupt handler table
   Handler_Table : array (Interrupt_Id) of Interrupt_Handler :=
     (others => null);

   -- Interrupt state tracking
   Interrupts_On : Boolean := False;

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Handler
     (Vector  : Interrupt_Id;
      Handler : Interrupt_Handler)
   is
   begin
      Handler_Table (Vector) := Handler;
      -- In actual implementation, would also update hardware vector table
   end Install_Handler;

   --------------------
   -- Remove_Handler --
   --------------------

   procedure Remove_Handler (Vector : Interrupt_Id) is
   begin
      Handler_Table (Vector) := null;
   end Remove_Handler;

   -----------------
   -- Get_Handler --
   -----------------

   function Get_Handler (Vector : Interrupt_Id) return Interrupt_Handler is
   begin
      return Handler_Table (Vector);
   end Get_Handler;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts is
   begin
      -- In actual Z80 code: EI instruction
      Interrupts_On := True;
   end Enable_Interrupts;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts is
   begin
      -- In actual Z80 code: DI instruction
      Interrupts_On := False;
   end Disable_Interrupts;

   ------------------------
   -- Interrupts_Enabled --
   ------------------------

   function Interrupts_Enabled return Boolean is
   begin
      return Interrupts_On;
   end Interrupts_Enabled;

end System.Interrupt_Handlers;
