-- System.Task_Primitives body for Z80
-- Low-level tasking primitives for preemptive multitasking

package body System.Task_Primitives is

   -- Task table
   Tasks : array (Task_Id range 1 .. Max_Tasks) of Task_Control_Block;

   -- Currently running task
   Current : Task_Id := Null_Task;

   -- Next available task ID
   Next_Id : Task_Id := Main_Task_Id + 1;

   -- Tasking system initialized flag
   Initialized : Boolean := False;

   -- Interrupt nesting counter for critical sections
   Critical_Nesting : Natural := 0;

   -- Saved interrupt state
   Saved_IFF : Boolean := True;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if Initialized then
         return;
      end if;

      -- Initialize all TCBs
      for I in Tasks'Range loop
         Tasks (I).Id := Null_Task;
         Tasks (I).State := Created;
      end loop;

      -- Set up main task (task 1)
      Tasks (Main_Task_Id).Id := Main_Task_Id;
      Tasks (Main_Task_Id).State := Running;
      Tasks (Main_Task_Id).Priority := Default_Priority;

      Current := Main_Task_Id;
      Initialized := True;
   end Initialize;

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
      if not Initialized then
         Initialize;
      end if;

      if Next_Id > Max_Tasks then
         Id := Null_Task;
         raise Program_Error;  -- Too many tasks
      end if;

      Id := Next_Id;
      Next_Id := Next_Id + 1;

      Tasks (Id).Id := Id;
      Tasks (Id).State := Created;
      Tasks (Id).Priority := Priority;
      Tasks (Id).Stack_Base := Stack_Base;
      Tasks (Id).Stack_Size := Stack_Size;
      Tasks (Id).Entry_Point := Entry_Point;

      -- Initialize context
      -- SP points to top of stack
      Tasks (Id).Context.SP := Natural (Stack_Base) + Stack_Size - 2;
      Tasks (Id).Context.PC := Natural (Entry_Point);
      Tasks (Id).Context.AF := 0;
      Tasks (Id).Context.BC := 0;
      Tasks (Id).Context.DE := 0;
      Tasks (Id).Context.HL := 0;
      Tasks (Id).Context.IX := 0;
      Tasks (Id).Context.IY := 0;
      Tasks (Id).Context.IFF1 := True;  -- Interrupts enabled
      Tasks (Id).Context.IFF2 := True;
   end Create_Task;

   ----------------
   -- Start_Task --
   ----------------

   procedure Start_Task (Id : Task_Id) is
   begin
      if Id = Null_Task or else Id > Max_Tasks then
         return;
      end if;

      if Tasks (Id).State = Created then
         Tasks (Id).State := Ready;
      end if;
   end Start_Task;

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
   begin
      -- Find next ready task with highest priority
      declare
         Best : Task_Id := Null_Task;
         Best_Prio : Task_Priority := Task_Priority'First;
      begin
         for I in Tasks'Range loop
            if Tasks (I).State = Ready
              and then Tasks (I).Priority > Best_Prio
            then
               Best := I;
               Best_Prio := Tasks (I).Priority;
            end if;
         end loop;

         if Best /= Null_Task then
            -- Context switch to Best
            Tasks (Current).State := Ready;
            Current := Best;
            Tasks (Current).State := Running;
            -- Actual context switch would be done in assembly
         end if;
      end;
   end Yield;

   -------------
   -- Suspend --
   -------------

   procedure Suspend (Id : Task_Id) is
   begin
      if Id = Null_Task or else Id > Max_Tasks then
         return;
      end if;

      Enter_Critical;
      if Tasks (Id).State = Running or else Tasks (Id).State = Ready then
         Tasks (Id).State := Suspended;
         if Id = Current then
            Yield;
         end if;
      end if;
      Leave_Critical;
   end Suspend;

   ------------
   -- Resume --
   ------------

   procedure Resume (Id : Task_Id) is
   begin
      if Id = Null_Task or else Id > Max_Tasks then
         return;
      end if;

      Enter_Critical;
      if Tasks (Id).State = Suspended then
         Tasks (Id).State := Ready;
      end if;
      Leave_Critical;
   end Resume;

   --------------------
   -- Terminate_Self --
   --------------------

   procedure Terminate_Self is
   begin
      Enter_Critical;
      Tasks (Current).State := Terminated;
      Leave_Critical;

      -- Switch to another task
      Yield;

      -- Should not reach here
      loop
         null;
      end loop;
   end Terminate_Self;

   ---------------
   -- Get_State --
   ---------------

   function Get_State (Id : Task_Id) return Task_State is
   begin
      if Id = Null_Task or else Id > Max_Tasks then
         return Terminated;
      end if;
      return Tasks (Id).State;
   end Get_State;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority (Id : Task_Id; Priority : Task_Priority) is
   begin
      if Id = Null_Task or else Id > Max_Tasks then
         return;
      end if;
      Tasks (Id).Priority := Priority;
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (Id : Task_Id) return Task_Priority is
   begin
      if Id = Null_Task or else Id > Max_Tasks then
         return Task_Priority'First;
      end if;
      return Tasks (Id).Priority;
   end Get_Priority;

   --------------------
   -- Enter_Critical --
   --------------------

   procedure Enter_Critical is
   begin
      if Critical_Nesting = 0 then
         -- Save interrupt state and disable
         -- In Z80: DI instruction
         Saved_IFF := True;  -- Would read actual IFF
         -- DI
      end if;
      Critical_Nesting := Critical_Nesting + 1;
   end Enter_Critical;

   --------------------
   -- Leave_Critical --
   --------------------

   procedure Leave_Critical is
   begin
      if Critical_Nesting > 0 then
         Critical_Nesting := Critical_Nesting - 1;
         if Critical_Nesting = 0 and then Saved_IFF then
            -- Restore interrupts
            -- In Z80: EI instruction
            null;  -- EI
         end if;
      end if;
   end Leave_Critical;

   ------------------------
   -- Timer_Tick_Handler --
   ------------------------

   procedure Timer_Tick_Handler is
   begin
      -- Called from RST 10h interrupt
      -- Save current task context (done in asm wrapper)
      -- Run scheduler
      if Initialized then
         -- Simple round-robin with priority
         Yield;
      end if;
      -- Restore next task context (done in asm wrapper)
   end Timer_Tick_Handler;

   -----------------
   -- NMI_Handler --
   -----------------

   procedure NMI_Handler is
   begin
      -- Called from NMI at 0066h
      -- NMI cannot be disabled - use for emergency/watchdog
      null;
   end NMI_Handler;

end System.Task_Primitives;
