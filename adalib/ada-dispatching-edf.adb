-- Ada.Dispatching.EDF body for Z80
-- Earliest Deadline First dispatching implementation

with System.Tasking;

package body Ada.Dispatching.EDF is

   -- Per-task deadlines (Max_Tasks = 8)
   Task_Deadlines : array (1 .. 8) of Deadline :=
     (others => Default_Deadline);

   ------------------
   -- Set_Deadline --
   ------------------

   procedure Set_Deadline
     (D : Deadline;
      T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task)
   is
      Index : constant Natural :=
        Natural (System.Tasking.To_Task_Id (T));
   begin
      if Index in Task_Deadlines'Range then
         Task_Deadlines (Index) := D;
      end if;
   end Set_Deadline;

   -----------------------------------
   -- Delay_Until_And_Set_Deadline --
   -----------------------------------

   procedure Delay_Until_And_Set_Deadline
     (Delay_Until_Time : Ada.Real_Time.Time;
      Deadline_Offset  : Ada.Real_Time.Time_Span)
   is
      use Ada.Real_Time;
   begin
      delay until Delay_Until_Time;
      Set_Deadline (Clock + Deadline_Offset);
   end Delay_Until_And_Set_Deadline;

   ------------------
   -- Get_Deadline --
   ------------------

   function Get_Deadline
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task)
      return Deadline
   is
      Index : constant Natural :=
        Natural (System.Tasking.To_Task_Id (T));
   begin
      if Index in Task_Deadlines'Range then
         return Task_Deadlines (Index);
      else
         return Default_Deadline;
      end if;
   end Get_Deadline;

end Ada.Dispatching.EDF;
