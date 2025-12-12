-- Ada.Execution_Time.Interrupts for Z80
-- Interrupt execution time tracking (Ada 2012)

with Ada.Interrupts;

package Ada.Execution_Time.Interrupts is
   pragma Preelaborate;

   -- Get execution time for a specific interrupt
   function Clock (Interrupt : Ada.Interrupts.Interrupt_Id)
     return CPU_Time;

   -- Check if interrupt time is supported
   function Supported (Interrupt : Ada.Interrupts.Interrupt_Id)
     return Boolean;

end Ada.Execution_Time.Interrupts;
