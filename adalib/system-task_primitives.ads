-- System.Task_Primitives for Z80
-- Low-level tasking primitives for preemptive multitasking
--
-- Uses cpmemu's interrupt support:
--   --mask-interrupt <range> rst 2   (timer tick at RST 10h)
--   --nmi <range>                    (high priority at 0066h)

package System.Task_Primitives is
   pragma Preelaborate;

   -- Maximum number of concurrent tasks (limited by Z80 memory)
   Max_Tasks : constant := 8;

   -- Task priority levels
   type Task_Priority is range 0 .. 15;
   Default_Priority : constant Task_Priority := 7;

   -- Task states
   type Task_State is (
      Created,      -- Task created but not started
      Ready,        -- Ready to run
      Running,      -- Currently executing
      Blocked,      -- Waiting for event
      Suspended,    -- Explicitly suspended
      Terminated);  -- Task has completed

   -- Task ID
   type Task_Id is range 0 .. Max_Tasks;
   Null_Task : constant Task_Id := 0;
   Main_Task_Id : constant Task_Id := 1;

   -- Z80 register context for task switching
   type Task_Context is record
      -- Main register set
      AF : Natural;  -- Accumulator and Flags
      BC : Natural;
      DE : Natural;
      HL : Natural;
      -- Alternate register set
      AF_Prime : Natural;
      BC_Prime : Natural;
      DE_Prime : Natural;
      HL_Prime : Natural;
      -- Index registers
      IX : Natural;
      IY : Natural;
      -- Stack pointer and program counter
      SP : Natural;
      PC : Natural;
      -- Interrupt state
      IFF1 : Boolean;  -- Interrupt flip-flop 1
      IFF2 : Boolean;  -- Interrupt flip-flop 2
   end record;

   -- Task Control Block
   type Task_Control_Block is record
      Id          : Task_Id := Null_Task;
      State       : Task_State := Created;
      Priority    : Task_Priority := Default_Priority;
      Context     : Task_Context;
      Stack_Base  : System.Address := System.Null_Address;
      Stack_Size  : Natural := 0;
      Entry_Point : System.Address := System.Null_Address;
   end record;

   -- Initialize tasking system
   procedure Initialize;

   -- Create a new task
   procedure Create_Task
     (Id          : out Task_Id;
      Entry_Point : System.Address;
      Stack_Base  : System.Address;
      Stack_Size  : Natural;
      Priority    : Task_Priority := Default_Priority);

   -- Start a created task
   procedure Start_Task (Id : Task_Id);

   -- Get current running task
   function Current_Task return Task_Id;

   -- Yield processor to another task (cooperative)
   procedure Yield;

   -- Suspend a task
   procedure Suspend (Id : Task_Id);

   -- Resume a suspended task
   procedure Resume (Id : Task_Id);

   -- Terminate current task
   procedure Terminate_Self;

   -- Get task state
   function Get_State (Id : Task_Id) return Task_State;

   -- Set task priority
   procedure Set_Priority (Id : Task_Id; Priority : Task_Priority);

   -- Get task priority
   function Get_Priority (Id : Task_Id) return Task_Priority;

   -- Enter critical section (disable interrupts)
   procedure Enter_Critical;

   -- Leave critical section (restore interrupts)
   procedure Leave_Critical;

   -- Timer tick handler (called from interrupt)
   procedure Timer_Tick_Handler;
   pragma Export (C, Timer_Tick_Handler, "ada_timer_tick");

   -- NMI handler (called from NMI)
   procedure NMI_Handler;
   pragma Export (C, NMI_Handler, "ada_nmi_handler");

end System.Task_Primitives;
