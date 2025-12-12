-- Ada.Synchronous_Task_Control body for Z80
-- Task synchronization implementation with preemptive tasking
--
-- Uses System.Tasking for proper yielding between tasks

with System.Tasking;

package body Ada.Synchronous_Task_Control is

   --------------
   -- Set_True --
   --------------

   procedure Set_True (S : in Out Suspension_Object) is
   begin
      System.Tasking.Enter_Protected;
      S.State := True;
      System.Tasking.Leave_Protected;
   end Set_True;

   ---------------
   -- Set_False --
   ---------------

   procedure Set_False (S : in Out Suspension_Object) is
   begin
      System.Tasking.Enter_Protected;
      S.State := False;
      System.Tasking.Leave_Protected;
   end Set_False;

   -------------------
   -- Current_State --
   -------------------

   function Current_State (S : Suspension_Object) return Boolean is
      Result : Boolean;
   begin
      System.Tasking.Enter_Protected;
      Result := S.State;
      System.Tasking.Leave_Protected;
      return Result;
   end Current_State;

   -------------------------
   -- Suspend_Until_True --
   -------------------------

   procedure Suspend_Until_True (S : in Out Suspension_Object) is
   begin
      -- Wait until state becomes True, yielding to other tasks
      loop
         System.Tasking.Enter_Protected;
         if S.State then
            -- Reset to False and exit
            S.State := False;
            System.Tasking.Leave_Protected;
            exit;
         end if;
         System.Tasking.Leave_Protected;

         -- Yield to allow other tasks to run and potentially set True
         System.Tasking.Yield;
      end loop;
   end Suspend_Until_True;

end Ada.Synchronous_Task_Control;
