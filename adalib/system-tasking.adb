-- System.Tasking body for Z80
-- Tasking support with preemptive scheduling via interrupts

with System.Task_Primitives;

package body System.Tasking is

   use System.Task_Primitives;

   -- Map Task_Primitives state to Tasking state
   function Map_State (S : System.Task_Primitives.Task_State) return Task_State is
   begin
      case S is
         when System.Task_Primitives.Created =>
            return Activating;
         when System.Task_Primitives.Ready | System.Task_Primitives.Running =>
            return Runnable;
         when System.Task_Primitives.Blocked =>
            return Waiting;
         when System.Task_Primitives.Suspended =>
            return Suspended;
         when System.Task_Primitives.Terminated =>
            return Terminated;
      end case;
   end Map_State;

   ------------------
   -- Current_Task --
   ------------------

   function Current_Task return Task_Id is
   begin
      return System.Task_Primitives.Current_Task;
   end Current_Task;

   -----------
   -- State --
   -----------

   function State (T : Task_Id) return Task_State is
   begin
      return Map_State (System.Task_Primitives.Get_State (T));
   end State;

   --------------
   -- Callable --
   --------------

   function Callable (T : Task_Id) return Boolean is
      S : constant Task_State := State (T);
   begin
      return S = Runnable or else S = Waiting;
   end Callable;

   ----------------
   -- Terminated --
   ----------------

   function Terminated (T : Task_Id) return Boolean is
   begin
      return State (T) = Terminated;
   end Terminated;

   -------------------------
   -- Initialize_Tasking --
   -------------------------

   procedure Initialize_Tasking is
   begin
      System.Task_Primitives.Initialize;
   end Initialize_Tasking;

   -----------------
   -- Create_Task --
   -----------------

   procedure Create_Task
     (Id          : out Task_Id;
      Entry_Point : System.Address;
      Stack_Base  : System.Address;
      Stack_Size  : Natural;
      Priority    : Task_Priority := Default_Priority)
   is
   begin
      System.Task_Primitives.Create_Task
        (Id          => Id,
         Entry_Point => Entry_Point,
         Stack_Base  => Stack_Base,
         Stack_Size  => Stack_Size,
         Priority    => Priority);
   end Create_Task;

   -------------------
   -- Activate_Task --
   -------------------

   procedure Activate_Task (Id : Task_Id) is
   begin
      System.Task_Primitives.Start_Task (Id);
   end Activate_Task;

   -----------
   -- Yield --
   -----------

   procedure Yield is
   begin
      System.Task_Primitives.Yield;
   end Yield;

   ------------------
   -- Suspend_Task --
   ------------------

   procedure Suspend_Task (Id : Task_Id) is
   begin
      System.Task_Primitives.Suspend (Id);
   end Suspend_Task;

   -----------------
   -- Resume_Task --
   -----------------

   procedure Resume_Task (Id : Task_Id) is
   begin
      System.Task_Primitives.Resume (Id);
   end Resume_Task;

   -----------------------
   -- Set_Task_Priority --
   -----------------------

   procedure Set_Task_Priority (Id : Task_Id; Priority : Task_Priority) is
   begin
      System.Task_Primitives.Set_Priority (Id, Priority);
   end Set_Task_Priority;

   -----------------------
   -- Get_Task_Priority --
   -----------------------

   function Get_Task_Priority (Id : Task_Id) return Task_Priority is
   begin
      return System.Task_Primitives.Get_Priority (Id);
   end Get_Task_Priority;

   ------------------
   -- Delay_Ticks --
   ------------------

   procedure Delay_Ticks (Ticks : Natural) is
      -- Simple busy-wait delay for now
      -- TODO: proper delay queue with timer
      Count : Natural := Ticks;
   begin
      while Count > 0 loop
         Yield;
         Count := Count - 1;
      end loop;
   end Delay_Ticks;

   ---------------------
   -- Enter_Protected --
   ---------------------

   procedure Enter_Protected is
   begin
      System.Task_Primitives.Enter_Critical;
   end Enter_Protected;

   ---------------------
   -- Leave_Protected --
   ---------------------

   procedure Leave_Protected is
   begin
      System.Task_Primitives.Leave_Critical;
   end Leave_Protected;

end System.Tasking;
