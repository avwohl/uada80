-- System.BB.Interrupts for Z80
-- Bare board interrupt handling

package System.BB.Interrupts is
   pragma Preelaborate;

   -- Z80 RST vectors (addresses)
   RST_00 : constant := 16#0000#;
   RST_08 : constant := 16#0008#;
   RST_10 : constant := 16#0010#;
   RST_18 : constant := 16#0018#;
   RST_20 : constant := 16#0020#;
   RST_28 : constant := 16#0028#;
   RST_30 : constant := 16#0030#;
   RST_38 : constant := 16#0038#;
   NMI    : constant := 16#0066#;

   -- Interrupt handler type
   type Interrupt_Handler is access procedure;

   -- Install an interrupt handler
   procedure Install_Handler
     (Vector  : Natural;
      Handler : Interrupt_Handler);

   -- Remove an interrupt handler
   procedure Remove_Handler (Vector : Natural);

   -- Get current handler
   function Get_Handler (Vector : Natural) return Interrupt_Handler;

   -- Enable/Disable specific interrupt (for software tracking)
   procedure Enable_Interrupt (Vector : Natural);
   procedure Disable_Interrupt (Vector : Natural);

   -- Check if interrupt is enabled
   function Is_Enabled (Vector : Natural) return Boolean;

end System.BB.Interrupts;
