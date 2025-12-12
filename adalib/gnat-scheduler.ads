-- GNAT.Scheduler for Z80
-- Simple cooperative task scheduler

package GNAT.Scheduler is
   pragma Preelaborate;

   Max_Tasks : constant := 8;

   type Task_Id is range 0 .. Max_Tasks;
   No_Task : constant Task_Id := 0;

   type Task_State is (Idle, Ready, Running, Waiting, Suspended, Terminated);
   type Priority_Level is range 0 .. 15;
   Default_Priority : constant Priority_Level := 7;

   -- Task callback procedure type
   type Task_Procedure is access procedure;

   -- Initialize scheduler
   procedure Initialize;

   -- Create a task (returns No_Task if full)
   function Create_Task (Proc : Task_Procedure;
                         Prio : Priority_Level := Default_Priority) return Task_Id;

   -- Terminate a task
   procedure Terminate_Task (Id : Task_Id);

   -- Suspend/Resume
   procedure Suspend_Task (Id : Task_Id);
   procedure Resume_Task (Id : Task_Id);

   -- Change priority
   procedure Set_Priority (Id : Task_Id; Prio : Priority_Level);
   function Get_Priority (Id : Task_Id) return Priority_Level;

   -- Get task state
   function Get_State (Id : Task_Id) return Task_State;

   -- Current task
   function Current_Task return Task_Id;

   -- Yield to next task (cooperative)
   procedure Yield;

   -- Run scheduler (starts task execution)
   procedure Run;

   -- Stop scheduler
   procedure Stop;

   -- Is scheduler running?
   function Is_Running return Boolean;

   -- Task count
   function Task_Count return Natural;
   function Active_Task_Count return Natural;

   -- Wait for ticks (cooperative delay)
   procedure Delay_Ticks (Ticks : Positive);

   -- Wait for condition
   type Condition_Func is access function return Boolean;
   procedure Wait_For (Cond : Condition_Func; Timeout_Ticks : Natural := 0);

   -- Simple semaphore
   type Semaphore is limited private;
   procedure Init_Semaphore (Sem : out Semaphore; Initial : Natural := 1);
   procedure Wait (Sem : in Out Semaphore);
   procedure Signal (Sem : in Out Semaphore);
   function Try_Wait (Sem : in Out Semaphore) return Boolean;

   -- Simple mutex
   type Mutex is limited private;
   procedure Init_Mutex (M : out Mutex);
   procedure Lock (M : in Out Mutex);
   procedure Unlock (M : in Out Mutex);
   function Try_Lock (M : in Out Mutex) return Boolean;
   function Is_Locked (M : Mutex) return Boolean;

   -- Event flags
   type Event_Flags is limited private;
   Max_Events : constant := 8;

   procedure Init_Events (E : out Event_Flags);
   procedure Set_Event (E : in Out Event_Flags; Flag : Natural);
   procedure Clear_Event (E : in Out Event_Flags; Flag : Natural);
   procedure Clear_All_Events (E : out Event_Flags);
   function Test_Event (E : Event_Flags; Flag : Natural) return Boolean;
   function Any_Event_Set (E : Event_Flags) return Boolean;
   procedure Wait_Event (E : Event_Flags; Flag : Natural);
   procedure Wait_Any_Event (E : Event_Flags);

   -- Statistics
   function Total_Context_Switches return Natural;
   function Idle_Ticks return Natural;

private

   type Semaphore is limited record
      Count : Natural := 1;
   end record;

   type Mutex is limited record
      Locked : Boolean := False;
      Owner  : Task_Id := No_Task;
   end record;

   type Event_Flags is limited record
      Flags : Natural := 0;  -- Bit flags
   end record;

end GNAT.Scheduler;
