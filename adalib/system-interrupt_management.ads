-- System.Interrupt_Management for Z80
-- Interrupt handling management

package System.Interrupt_Management is
   pragma Preelaborate;

   -- Interrupt ID type
   type Interrupt_ID is range 0 .. 7;
   --  Z80 supports 8 interrupt sources in mode 2

   -- Special interrupt IDs
   NMI_Interrupt : constant Interrupt_ID := 0;  -- Non-maskable interrupt
   RST_08        : constant Interrupt_ID := 1;
   RST_10        : constant Interrupt_ID := 2;
   RST_18        : constant Interrupt_ID := 3;
   RST_20        : constant Interrupt_ID := 4;
   RST_28        : constant Interrupt_ID := 5;
   RST_30        : constant Interrupt_ID := 6;
   RST_38        : constant Interrupt_ID := 7;  -- Mode 1 interrupt

   -- Interrupt handler type
   type Interrupt_Handler is access procedure;
   pragma Favor_Top_Level (Interrupt_Handler);

   -- Register interrupt handler
   procedure Attach_Handler
     (Handler   : Interrupt_Handler;
      Interrupt : Interrupt_ID);

   -- Detach interrupt handler
   procedure Detach_Handler (Interrupt : Interrupt_ID);

   -- Enable/disable specific interrupt
   procedure Enable_Interrupt (Interrupt : Interrupt_ID);
   procedure Disable_Interrupt (Interrupt : Interrupt_ID);

   -- Global interrupt control
   procedure Enable_Interrupts;
   procedure Disable_Interrupts;

   -- Check if interrupts enabled
   function Interrupts_Enabled return Boolean;

   -- Initialize interrupt management
   procedure Initialize;

end System.Interrupt_Management;
