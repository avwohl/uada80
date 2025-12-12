-- Ada.Task_Termination for Z80
-- Task termination handlers

with Ada.Task_Identification;
with Ada.Exceptions;

package Ada.Task_Termination is
   pragma Preelaborate;

   type Cause_Of_Termination is (Normal, Abnormal, Unhandled_Exception);

   type Termination_Handler is access protected procedure
     (Cause : Cause_Of_Termination;
      T     : Ada.Task_Identification.Task_Id;
      X     : Ada.Exceptions.Exception_Occurrence);

   procedure Set_Dependents_Fallback_Handler (Handler : Termination_Handler);
   function Current_Task_Fallback_Handler return Termination_Handler;

   procedure Set_Specific_Handler
     (T       : Ada.Task_Identification.Task_Id;
      Handler : Termination_Handler);

   function Specific_Handler
     (T : Ada.Task_Identification.Task_Id) return Termination_Handler;

end Ada.Task_Termination;
