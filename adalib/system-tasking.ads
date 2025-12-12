-- System.Tasking for Z80
-- Tasking support with preemptive scheduling via interrupts
--
-- Requires cpmemu interrupt support:
--   --mask-interrupt <range> rst 2    (timer tick for scheduler)
--
-- Example: cpmemu --mask-interrupt 1000-2000 rst 2 program.com

with System.Task_Primitives;

package System.Tasking is
   pragma Preelaborate;

   -- Re-export from Task_Primitives for compatibility
   Max_Tasks : constant := System.Task_Primitives.Max_Tasks;

   -- Task identification
   subtype Task_Id is System.Task_Primitives.Task_Id;

   Null_Task : constant Task_Id := System.Task_Primitives.Null_Task;
   Main_Task : constant Task_Id := System.Task_Primitives.Main_Task_Id;

   -- Task priority
   subtype Task_Priority is System.Task_Primitives.Task_Priority;
   Default_Priority : constant Task_Priority :=
     System.Task_Primitives.Default_Priority;

   -- Task states
   type Task_State is (
      Runnable,     -- Ready or Running
      Terminated,   -- Task completed
      Activating,   -- Being created
      Waiting,      -- Blocked on entry/delay
      Suspended);   -- Explicitly suspended

   -- Get current task
   function Current_Task return Task_Id;

   -- Get task state
   function State (T : Task_Id) return Task_State;

   -- Check if task is callable (can accept entry calls)
   function Callable (T : Task_Id) return Boolean;

   -- Check if task is terminated
   function Terminated (T : Task_Id) return Boolean;

   -- Initialize tasking runtime
   procedure Initialize_Tasking;

   -- Create and start a task
   procedure Create_Task
     (Id          : out Task_Id;
      Entry_Point : System.Address;
      Stack_Base  : System.Address;
      Stack_Size  : Natural;
      Priority    : Task_Priority := Default_Priority);

   procedure Activate_Task (Id : Task_Id);

   -- Task control
   procedure Yield;
   procedure Suspend_Task (Id : Task_Id);
   procedure Resume_Task (Id : Task_Id);

   -- Priority control
   procedure Set_Task_Priority (Id : Task_Id; Priority : Task_Priority);
   function Get_Task_Priority (Id : Task_Id) return Task_Priority;

   -- Delay support (in timer ticks)
   procedure Delay_Ticks (Ticks : Natural);

   -- Critical sections for protected objects
   procedure Enter_Protected;
   procedure Leave_Protected;

end System.Tasking;
