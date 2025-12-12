-- Ada.Dispatching.EDF for Z80
-- Earliest Deadline First dispatching (Ada 2005)

with Ada.Real_Time;
with Ada.Task_Identification;

package Ada.Dispatching.EDF is
   pragma Preelaborate;

   subtype Deadline is Ada.Real_Time.Time;

   Default_Deadline : constant Deadline := Ada.Real_Time.Time_Last;

   -- Set deadline for specified task
   procedure Set_Deadline
     (D : Deadline;
      T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task);

   -- Delay until deadline
   procedure Delay_Until_And_Set_Deadline
     (Delay_Until_Time : Ada.Real_Time.Time;
      Deadline_Offset  : Ada.Real_Time.Time_Span);

   -- Query deadline
   function Get_Deadline
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task)
      return Deadline;

end Ada.Dispatching.EDF;
