-- System.Interrupt_Handlers for Z80
-- Interrupt handler support

package System.Interrupt_Handlers is
   pragma Preelaborate;

   -- Z80 interrupt vectors
   -- Mode 0: Not commonly used with interrupt table
   -- Mode 1: RST 38h (0038h)
   -- Mode 2: Vector table at I register * 256 + device byte

   type Interrupt_Id is range 0 .. 127;
   -- Z80 can have up to 128 interrupt vectors in Mode 2

   -- Standard Z80 interrupt handlers
   NMI_Vector   : constant := 16#0066#;  -- Non-maskable interrupt
   Mode1_Vector : constant := 16#0038#;  -- Mode 1 interrupt

   -- Interrupt handler procedure type
   type Interrupt_Handler is access procedure;
   pragma Convention (C, Interrupt_Handler);

   -- Install interrupt handler
   procedure Install_Handler
     (Vector  : Interrupt_Id;
      Handler : Interrupt_Handler);

   -- Remove interrupt handler
   procedure Remove_Handler (Vector : Interrupt_Id);

   -- Get current handler for vector
   function Get_Handler (Vector : Interrupt_Id) return Interrupt_Handler;

   -- Enable/disable interrupts
   procedure Enable_Interrupts;
   procedure Disable_Interrupts;

   -- Check if interrupts are enabled
   function Interrupts_Enabled return Boolean;

end System.Interrupt_Handlers;
