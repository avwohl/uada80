-- Ada.Execution_Time.Timers for Z80
-- Execution time timers (Ada 2012)

with Ada.Task_Identification;
with Ada.Real_Time;

package Ada.Execution_Time.Timers is
   pragma Preelaborate;

   type Timer (T : not null access constant
                     Ada.Task_Identification.Task_Id) is
      tagged limited private;

   type Timer_Handler is access protected procedure (TM : in out Timer);

   Min_Handler_Ceiling : constant System.Any_Priority :=
     System.Interrupt_Priority'Last;

   -- Set handler for timer
   procedure Set_Handler
     (TM      : in Out Timer;
      In_Time : Ada.Real_Time.Time_Span;
      Handler : Timer_Handler);

   procedure Set_Handler
     (TM      : in Out Timer;
      At_Time : CPU_Time;
      Handler : Timer_Handler);

   -- Query timer
   function Current_Handler (TM : Timer) return Timer_Handler;
   function Time_Remaining (TM : Timer) return Ada.Real_Time.Time_Span;

   -- Cancel timer
   procedure Cancel_Handler
     (TM        : in Out Timer;
      Cancelled : out Boolean);

   Timer_Resource_Error : exception;

private

   type Timer (T : not null access constant
                     Ada.Task_Identification.Task_Id) is
      tagged limited record
         Handler   : Timer_Handler := null;
         Expires   : CPU_Time := CPU_Time_First;
         Active    : Boolean := False;
      end record;

end Ada.Execution_Time.Timers;
