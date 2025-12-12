-- Ada.Asynchronous_Task_Control for Z80
-- Asynchronous task control operations

with Ada.Task_Identification;

package Ada.Asynchronous_Task_Control is
   pragma Preelaborate;

   procedure Hold (T : Ada.Task_Identification.Task_Id);
   procedure Continue (T : Ada.Task_Identification.Task_Id);
   function Is_Held (T : Ada.Task_Identification.Task_Id) return Boolean;

end Ada.Asynchronous_Task_Control;
