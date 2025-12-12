-- GNAT.Scheduler body for Z80
-- Cooperative task scheduler implementation

package body GNAT.Scheduler is

   type Task_Record is record
      Proc       : Task_Procedure;
      State      : Task_State;
      Priority   : Priority_Level;
      Wait_Ticks : Natural;
      Wait_Cond  : Condition_Func;
   end record;

   Tasks : array (1 .. Max_Tasks) of Task_Record;

   Current     : Task_Id := No_Task;
   Running_Flag : Boolean := False;
   Switches    : Natural := 0;
   Idle_Count  : Natural := 0;

   -- System tick counter (updated by system)
   Tick_Counter : Natural := 0;
   pragma Volatile (Tick_Counter);

   procedure Get_Tick is
      -- Import system tick
      pragma Import (Assembler, Get_Tick, "get_tick");
   begin
      null;
   end Get_Tick;

   function System_Tick return Natural is
   begin
      return Tick_Counter;
   end System_Tick;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      for I in 1 .. Max_Tasks loop
         Tasks (I).Proc := null;
         Tasks (I).State := Idle;
         Tasks (I).Priority := 0;
         Tasks (I).Wait_Ticks := 0;
         Tasks (I).Wait_Cond := null;
      end loop;

      Current := No_Task;
      Running_Flag := False;
      Switches := 0;
      Idle_Count := 0;
   end Initialize;

   -----------------
   -- Create_Task --
   -----------------

   function Create_Task (Proc : Task_Procedure;
                         Prio : Priority_Level := Default_Priority) return Task_Id is
   begin
      for I in 1 .. Max_Tasks loop
         if Tasks (I).State = Idle then
            Tasks (I).Proc := Proc;
            Tasks (I).State := Ready;
            Tasks (I).Priority := Prio;
            Tasks (I).Wait_Ticks := 0;
            Tasks (I).Wait_Cond := null;
            return Task_Id (I);
         end if;
      end loop;
      return No_Task;
   end Create_Task;

   --------------------
   -- Terminate_Task --
   --------------------

   procedure Terminate_Task (Id : Task_Id) is
   begin
      if Id in 1 .. Max_Tasks then
         Tasks (Natural (Id)).State := Terminated;
         Tasks (Natural (Id)).Proc := null;
      end if;
   end Terminate_Task;

   ------------------
   -- Suspend_Task --
   ------------------

   procedure Suspend_Task (Id : Task_Id) is
   begin
      if Id in 1 .. Max_Tasks then
         if Tasks (Natural (Id)).State = Ready or
            Tasks (Natural (Id)).State = Running then
            Tasks (Natural (Id)).State := Suspended;
         end if;
      end if;
   end Suspend_Task;

   -----------------
   -- Resume_Task --
   -----------------

   procedure Resume_Task (Id : Task_Id) is
   begin
      if Id in 1 .. Max_Tasks then
         if Tasks (Natural (Id)).State = Suspended then
            Tasks (Natural (Id)).State := Ready;
         end if;
      end if;
   end Resume_Task;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority (Id : Task_Id; Prio : Priority_Level) is
   begin
      if Id in 1 .. Max_Tasks then
         Tasks (Natural (Id)).Priority := Prio;
      end if;
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (Id : Task_Id) return Priority_Level is
   begin
      if Id in 1 .. Max_Tasks then
         return Tasks (Natural (Id)).Priority;
      else
         return 0;
      end if;
   end Get_Priority;

   ---------------
   -- Get_State --
   ---------------

   function Get_State (Id : Task_Id) return Task_State is
   begin
      if Id in 1 .. Max_Tasks then
         return Tasks (Natural (Id)).State;
      else
         return Idle;
      end if;
   end Get_State;

   ------------------
   -- Current_Task --
   ------------------

   function Current_Task return Task_Id is
   begin
      return Current;
   end Current_Task;

   -----------
   -- Yield --
   -----------

   procedure Yield is
      Best_Task : Task_Id := No_Task;
      Best_Prio : Priority_Level := 0;
      Start_Tick : Natural;
   begin
      if not Running_Flag then
         return;
      end if;

      -- Mark current task as ready (if running)
      if Current /= No_Task then
         if Tasks (Natural (Current)).State = Running then
            Tasks (Natural (Current)).State := Ready;
         end if;
      end if;

      -- Update waiting tasks
      Start_Tick := System_Tick;
      for I in 1 .. Max_Tasks loop
         if Tasks (I).State = Waiting then
            -- Check tick delay
            if Tasks (I).Wait_Ticks > 0 then
               Tasks (I).Wait_Ticks := Tasks (I).Wait_Ticks - 1;
               if Tasks (I).Wait_Ticks = 0 then
                  Tasks (I).State := Ready;
               end if;
            end if;

            -- Check condition
            if Tasks (I).Wait_Cond /= null then
               if Tasks (I).Wait_Cond.all then
                  Tasks (I).Wait_Cond := null;
                  Tasks (I).State := Ready;
               end if;
            end if;
         end if;
      end loop;

      -- Find highest priority ready task
      for I in 1 .. Max_Tasks loop
         if Tasks (I).State = Ready then
            if Best_Task = No_Task or else Tasks (I).Priority > Best_Prio then
               Best_Task := Task_Id (I);
               Best_Prio := Tasks (I).Priority;
            end if;
         end if;
      end loop;

      -- Context switch
      if Best_Task /= No_Task then
         if Best_Task /= Current then
            Switches := Switches + 1;
         end if;
         Current := Best_Task;
         Tasks (Natural (Current)).State := Running;

         -- Execute task
         if Tasks (Natural (Current)).Proc /= null then
            Tasks (Natural (Current)).Proc.all;
         end if;
      else
         Idle_Count := Idle_Count + 1;
      end if;
   end Yield;

   ---------
   -- Run --
   ---------

   procedure Run is
   begin
      Running_Flag := True;

      while Running_Flag loop
         Yield;

         -- Check if all tasks terminated
         declare
            Any_Active : Boolean := False;
         begin
            for I in 1 .. Max_Tasks loop
               if Tasks (I).State /= Idle and
                  Tasks (I).State /= Terminated then
                  Any_Active := True;
                  exit;
               end if;
            end loop;

            if not Any_Active then
               Running_Flag := False;
            end if;
         end;
      end loop;
   end Run;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Running_Flag := False;
   end Stop;

   ----------------
   -- Is_Running --
   ----------------

   function Is_Running return Boolean is
   begin
      return Running_Flag;
   end Is_Running;

   ----------------
   -- Task_Count --
   ----------------

   function Task_Count return Natural is
      Count : Natural := 0;
   begin
      for I in 1 .. Max_Tasks loop
         if Tasks (I).State /= Idle then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Task_Count;

   -----------------------
   -- Active_Task_Count --
   -----------------------

   function Active_Task_Count return Natural is
      Count : Natural := 0;
   begin
      for I in 1 .. Max_Tasks loop
         if Tasks (I).State = Ready or
            Tasks (I).State = Running or
            Tasks (I).State = Waiting then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Active_Task_Count;

   -----------------
   -- Delay_Ticks --
   -----------------

   procedure Delay_Ticks (Ticks : Positive) is
   begin
      if Current /= No_Task then
         Tasks (Natural (Current)).State := Waiting;
         Tasks (Natural (Current)).Wait_Ticks := Ticks;
         Yield;
      end if;
   end Delay_Ticks;

   --------------
   -- Wait_For --
   --------------

   procedure Wait_For (Cond : Condition_Func; Timeout_Ticks : Natural := 0) is
   begin
      if Current /= No_Task and Cond /= null then
         -- Check if already true
         if Cond.all then
            return;
         end if;

         Tasks (Natural (Current)).State := Waiting;
         Tasks (Natural (Current)).Wait_Cond := Cond;
         Tasks (Natural (Current)).Wait_Ticks := Timeout_Ticks;
         Yield;
      end if;
   end Wait_For;

   --------------------
   -- Init_Semaphore --
   --------------------

   procedure Init_Semaphore (Sem : out Semaphore; Initial : Natural := 1) is
   begin
      Sem.Count := Initial;
   end Init_Semaphore;

   ----------
   -- Wait --
   ----------

   procedure Wait (Sem : in Out Semaphore) is
   begin
      while Sem.Count = 0 loop
         Yield;
      end loop;
      Sem.Count := Sem.Count - 1;
   end Wait;

   ------------
   -- Signal --
   ------------

   procedure Signal (Sem : in Out Semaphore) is
   begin
      Sem.Count := Sem.Count + 1;
   end Signal;

   --------------
   -- Try_Wait --
   --------------

   function Try_Wait (Sem : in Out Semaphore) return Boolean is
   begin
      if Sem.Count > 0 then
         Sem.Count := Sem.Count - 1;
         return True;
      else
         return False;
      end if;
   end Try_Wait;

   ----------------
   -- Init_Mutex --
   ----------------

   procedure Init_Mutex (M : out Mutex) is
   begin
      M.Locked := False;
      M.Owner := No_Task;
   end Init_Mutex;

   ----------
   -- Lock --
   ----------

   procedure Lock (M : in Out Mutex) is
   begin
      while M.Locked and M.Owner /= Current loop
         Yield;
      end loop;
      M.Locked := True;
      M.Owner := Current;
   end Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (M : in Out Mutex) is
   begin
      if M.Owner = Current then
         M.Locked := False;
         M.Owner := No_Task;
      end if;
   end Unlock;

   --------------
   -- Try_Lock --
   --------------

   function Try_Lock (M : in Out Mutex) return Boolean is
   begin
      if not M.Locked then
         M.Locked := True;
         M.Owner := Current;
         return True;
      elsif M.Owner = Current then
         return True;  -- Already own it
      else
         return False;
      end if;
   end Try_Lock;

   ---------------
   -- Is_Locked --
   ---------------

   function Is_Locked (M : Mutex) return Boolean is
   begin
      return M.Locked;
   end Is_Locked;

   -----------------
   -- Init_Events --
   -----------------

   procedure Init_Events (E : out Event_Flags) is
   begin
      E.Flags := 0;
   end Init_Events;

   ---------------
   -- Set_Event --
   ---------------

   procedure Set_Event (E : in Out Event_Flags; Flag : Natural) is
      Bit : Natural;
   begin
      if Flag < Max_Events then
         Bit := 1;
         for I in 1 .. Flag loop
            Bit := Bit * 2;
         end loop;
         E.Flags := E.Flags or Bit;
      end if;
   end Set_Event;

   -----------------
   -- Clear_Event --
   -----------------

   procedure Clear_Event (E : in Out Event_Flags; Flag : Natural) is
      Bit : Natural;
      Mask : Natural;
   begin
      if Flag < Max_Events then
         Bit := 1;
         for I in 1 .. Flag loop
            Bit := Bit * 2;
         end loop;
         Mask := 16#FFFF# - Bit;
         E.Flags := E.Flags and Mask;
      end if;
   end Clear_Event;

   ----------------------
   -- Clear_All_Events --
   ----------------------

   procedure Clear_All_Events (E : out Event_Flags) is
   begin
      E.Flags := 0;
   end Clear_All_Events;

   ----------------
   -- Test_Event --
   ----------------

   function Test_Event (E : Event_Flags; Flag : Natural) return Boolean is
      Bit : Natural;
   begin
      if Flag >= Max_Events then
         return False;
      end if;

      Bit := 1;
      for I in 1 .. Flag loop
         Bit := Bit * 2;
      end loop;

      return (E.Flags and Bit) /= 0;
   end Test_Event;

   -------------------
   -- Any_Event_Set --
   -------------------

   function Any_Event_Set (E : Event_Flags) return Boolean is
   begin
      return E.Flags /= 0;
   end Any_Event_Set;

   ----------------
   -- Wait_Event --
   ----------------

   procedure Wait_Event (E : Event_Flags; Flag : Natural) is
   begin
      while not Test_Event (E, Flag) loop
         Yield;
      end loop;
   end Wait_Event;

   --------------------
   -- Wait_Any_Event --
   --------------------

   procedure Wait_Any_Event (E : Event_Flags) is
   begin
      while not Any_Event_Set (E) loop
         Yield;
      end loop;
   end Wait_Any_Event;

   ----------------------------
   -- Total_Context_Switches --
   ----------------------------

   function Total_Context_Switches return Natural is
   begin
      return Switches;
   end Total_Context_Switches;

   ----------------
   -- Idle_Ticks --
   ----------------

   function Idle_Ticks return Natural is
   begin
      return Idle_Count;
   end Idle_Ticks;

end GNAT.Scheduler;
