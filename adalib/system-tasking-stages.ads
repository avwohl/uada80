-- System.Tasking.Stages for Z80
-- Task lifecycle stages

package System.Tasking.Stages is
   pragma Preelaborate;

   -- Task activation stages
   type Activation_Stage is (
      Not_Created,
      Created,
      Activating,
      Active,
      Complete,
      Terminated);

   -- Create a task (called by elaboration code)
   procedure Create_Task
     (Task_Image  : String;
      Entry_Point : System.Address;
      Stack_Size  : Natural;
      Priority    : Task_Priority;
      Task_Ptr    : out Task_Id);

   -- Activate a created task (start execution)
   procedure Activate_Tasks (Chain : Task_Id);

   -- Wait for task activation to complete
   procedure Complete_Activation;

   -- Complete a task (normal termination)
   procedure Complete_Task;

   -- Abort a task
   procedure Abort_Tasks (Chain : Task_Id);

   -- Wait for dependent tasks to terminate
   procedure Wait_For_Dependents;

   -- Get activation stage
   function Get_Stage (T : Task_Id) return Activation_Stage;

   -- Master completion
   procedure Enter_Master;
   procedure Complete_Master;
   function Master_Level return Natural;

end System.Tasking.Stages;
