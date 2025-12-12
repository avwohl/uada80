-- Ada.Dynamic_Priorities body for Z80
-- Dynamic task priority management with real tasking

with System.Tasking;

package body Ada.Dynamic_Priorities is

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (Priority : System.Any_Priority;
      T        : Ada.Task_Identification.Task_Id :=
                   Ada.Task_Identification.Current_Task)
   is
      Task_Prio : System.Tasking.Task_Priority;
   begin
      -- Map System.Any_Priority to Task_Priority
      -- Any_Priority is 0..15, Task_Priority is 0..15
      if Priority > System.Tasking.Task_Priority'Last then
         Task_Prio := System.Tasking.Task_Priority'Last;
      else
         Task_Prio := System.Tasking.Task_Priority (Priority);
      end if;

      System.Tasking.Set_Task_Priority
        (System.Tasking.Task_Id (T), Task_Prio);
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task)
     return System.Any_Priority
   is
      Task_Prio : constant System.Tasking.Task_Priority :=
        System.Tasking.Get_Task_Priority (System.Tasking.Task_Id (T));
   begin
      return System.Any_Priority (Task_Prio);
   end Get_Priority;

end Ada.Dynamic_Priorities;
