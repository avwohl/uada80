-- Ada.Task_Identification body for Z80
-- Task identification support implementation with real tasking

with System.Tasking;

package body Ada.Task_Identification is

   Main_Task_Id : constant Task_Id := 1;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Task_Id) return Boolean is
   begin
      return Natural (Left) = Natural (Right);
   end "=";

   -----------
   -- Image --
   -----------

   function Image (T : Task_Id) return String is
   begin
      if T = Null_Task_Id then
         return "null task";
      elsif T = Main_Task_Id then
         return "main task";
      else
         return "task" & Natural'Image (Natural (T));
      end if;
   end Image;

   ------------------
   -- Current_Task --
   ------------------

   function Current_Task return Task_Id is
   begin
      return Task_Id (System.Tasking.Current_Task);
   end Current_Task;

   ----------------------
   -- Environment_Task --
   ----------------------

   function Environment_Task return Task_Id is
   begin
      return Main_Task_Id;
   end Environment_Task;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : Task_Id) is
   begin
      if T = Null_Task_Id then
         raise Tasking_Error;
      end if;

      -- Abort by suspending the task
      -- Full abort semantics would require more infrastructure
      System.Tasking.Suspend_Task (System.Tasking.Task_Id (T));
   end Abort_Task;

   -------------------
   -- Is_Terminated --
   -------------------

   function Is_Terminated (T : Task_Id) return Boolean is
   begin
      if T = Null_Task_Id then
         raise Tasking_Error;
      end if;
      return System.Tasking.Terminated (System.Tasking.Task_Id (T));
   end Is_Terminated;

   -----------------
   -- Is_Callable --
   -----------------

   function Is_Callable (T : Task_Id) return Boolean is
   begin
      if T = Null_Task_Id then
         raise Tasking_Error;
      end if;
      return System.Tasking.Callable (System.Tasking.Task_Id (T));
   end Is_Callable;

   ------------------------------
   -- Activation_Is_Complete --
   ------------------------------

   function Activation_Is_Complete (T : Task_Id) return Boolean is
      use System.Tasking;
   begin
      if T = Null_Task_Id then
         raise Tasking_Error;
      end if;

      -- Task is activated if it's running or beyond
      return State (System.Tasking.Task_Id (T)) /= Activating;
   end Activation_Is_Complete;

end Ada.Task_Identification;
