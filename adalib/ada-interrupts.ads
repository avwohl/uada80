-- Ada.Interrupts for Z80
-- Interrupt handling support

with System;

package Ada.Interrupts is
   pragma Preelaborate;

   -- Z80 has 8 interrupt modes, we model RST interrupts
   type Interrupt_Id is range 0 .. 7;

   type Parameterless_Handler is access protected procedure;

   function Is_Reserved (Interrupt : Interrupt_Id) return Boolean;
   function Is_Attached (Interrupt : Interrupt_Id) return Boolean;

   function Current_Handler (Interrupt : Interrupt_Id) return Parameterless_Handler;

   procedure Attach_Handler
     (New_Handler : Parameterless_Handler;
      Interrupt   : Interrupt_Id);

   procedure Exchange_Handler
     (Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt   : Interrupt_Id);

   procedure Detach_Handler (Interrupt : Interrupt_Id);

   function Reference (Interrupt : Interrupt_Id) return System.Address;

private

   pragma Import (Intrinsic, Reference);

end Ada.Interrupts;
