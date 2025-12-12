-- Ada.Asynchronous_Task_Control body for Z80
-- Asynchronous task control operations with real tasking

with System.Tasking;

package body Ada.Asynchronous_Task_Control is

   ----------
   -- Hold --
   ----------

   procedure Hold (T : Ada.Task_Identification.Task_Id) is
      use Ada.Task_Identification;
   begin
      if T = Null_Task_Id then
         raise Program_Error;
      end if;

      -- Cannot hold the environment (main) task
      if T = Environment_Task then
         raise Tasking_Error;
      end if;

      System.Tasking.Suspend_Task (System.Tasking.Task_Id (T));
   end Hold;

   --------------
   -- Continue --
   --------------

   procedure Continue (T : Ada.Task_Identification.Task_Id) is
      use Ada.Task_Identification;
   begin
      if T = Null_Task_Id then
         raise Program_Error;
      end if;

      System.Tasking.Resume_Task (System.Tasking.Task_Id (T));
   end Continue;

   -------------
   -- Is_Held --
   -------------

   function Is_Held (T : Ada.Task_Identification.Task_Id) return Boolean is
      use Ada.Task_Identification;
      use System.Tasking;
   begin
      if T = Null_Task_Id then
         raise Program_Error;
      end if;

      return State (System.Tasking.Task_Id (T)) = Suspended;
   end Is_Held;

end Ada.Asynchronous_Task_Control;
