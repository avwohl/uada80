-- System.Tasking.Entry_Calls body for Z80
-- Runtime support for task entry calls (rendezvous)

package body System.Tasking.Entry_Calls is

   -- Global entry queues for all tasks
   Queues : array (Task_Id range 1 .. Max_Tasks) of Entry_Queue;

   -- Pending call for each task (when it's a caller)
   Pending_Calls : array (Task_Id range 1 .. Max_Tasks) of Entry_Call;

   ------------------------
   -- Initialize_Entries --
   ------------------------

   procedure Initialize_Entries (Queue : out Entry_Queue) is
   begin
      for I in Queue'Range loop
         Queue (I).Caller := Null_Task;
         Queue (I).Entry_Num := No_Entry;
         Queue (I).Accepted := False;
         Queue (I).Completed := False;
      end loop;
   end Initialize_Entries;

   ----------------
   -- Call_Entry --
   ----------------

   procedure Call_Entry
     (Callee    : Task_Id;
      Entry_Num : Entry_Index;
      Mode      : Call_Mode := Simple_Call)
   is
      Self : constant Task_Id := Current_Task;
      Slot : Natural := 0;
   begin
      if Callee = Null_Task or else Entry_Num = No_Entry then
         raise Program_Error;
      end if;

      Enter_Protected;

      -- Find free slot in callee's queue
      for I in Queues (Callee)'Range loop
         if Queues (Callee)(I).Caller = Null_Task then
            Slot := I;
            exit;
         end if;
      end loop;

      if Slot = 0 then
         Leave_Protected;
         raise Program_Error;  -- Queue full
      end if;

      -- Add call to queue
      Queues (Callee)(Slot).Caller := Self;
      Queues (Callee)(Slot).Entry_Num := Entry_Num;
      Queues (Callee)(Slot).Mode := Mode;
      Queues (Callee)(Slot).Accepted := False;
      Queues (Callee)(Slot).Completed := False;

      -- Track our pending call
      Pending_Calls (Self) := Queues (Callee)(Slot);

      Leave_Protected;

      -- Wait for completion (for simple calls)
      if Mode = Simple_Call then
         -- Block until accepted and completed
         loop
            Enter_Protected;
            if Pending_Calls (Self).Completed then
               Pending_Calls (Self).Caller := Null_Task;
               Leave_Protected;
               exit;
            end if;
            Leave_Protected;
            Yield;
         end loop;
      elsif Mode = Conditional_Call then
         -- Give callee one chance to accept
         Yield;
      end if;
   end Call_Entry;

   ------------------
   -- Accept_Entry --
   ------------------

   procedure Accept_Entry
     (Entry_Num : Entry_Index;
      Caller    : out Task_Id)
   is
      Self : constant Task_Id := Current_Task;
   begin
      Caller := Null_Task;

      -- Wait for a caller
      loop
         Enter_Protected;

         -- Search queue for matching entry call
         for I in Queues (Self)'Range loop
            if Queues (Self)(I).Caller /= Null_Task
              and then Queues (Self)(I).Entry_Num = Entry_Num
              and then not Queues (Self)(I).Accepted
            then
               -- Found one - mark as accepted
               Queues (Self)(I).Accepted := True;
               Caller := Queues (Self)(I).Caller;
               Pending_Calls (Caller).Accepted := True;
               Leave_Protected;
               return;
            end if;
         end loop;

         Leave_Protected;
         Yield;
      end loop;
   end Accept_Entry;

   --------------------
   -- Complete_Entry --
   --------------------

   procedure Complete_Entry (Caller : Task_Id) is
      Self : constant Task_Id := Current_Task;
   begin
      if Caller = Null_Task then
         return;
      end if;

      Enter_Protected;

      -- Find and remove the entry call
      for I in Queues (Self)'Range loop
         if Queues (Self)(I).Caller = Caller then
            Queues (Self)(I).Completed := True;
            Queues (Self)(I).Caller := Null_Task;
            Pending_Calls (Caller).Completed := True;
            exit;
         end if;
      end loop;

      Leave_Protected;
   end Complete_Entry;

   -----------------
   -- Entry_Count --
   -----------------

   function Entry_Count (T : Task_Id; Entry_Num : Entry_Index) return Natural is
      Count : Natural := 0;
   begin
      if T = Null_Task then
         return 0;
      end if;

      Enter_Protected;

      for I in Queues (T)'Range loop
         if Queues (T)(I).Caller /= Null_Task
           and then Queues (T)(I).Entry_Num = Entry_Num
           and then not Queues (T)(I).Accepted
         then
            Count := Count + 1;
         end if;
      end loop;

      Leave_Protected;
      return Count;
   end Entry_Count;

   -----------------------
   -- Cancel_Entry_Call --
   -----------------------

   procedure Cancel_Entry_Call (Caller : Task_Id) is
   begin
      Enter_Protected;

      -- Remove from all queues
      for T in Queues'Range loop
         for I in Queues (T)'Range loop
            if Queues (T)(I).Caller = Caller
              and then not Queues (T)(I).Accepted
            then
               Queues (T)(I).Caller := Null_Task;
            end if;
         end loop;
      end loop;

      Pending_Calls (Caller).Caller := Null_Task;

      Leave_Protected;
   end Cancel_Entry_Call;

   -------------------
   -- Call_Accepted --
   -------------------

   function Call_Accepted (Caller : Task_Id) return Boolean is
      Result : Boolean;
   begin
      Enter_Protected;
      Result := Pending_Calls (Caller).Accepted;
      Leave_Protected;
      return Result;
   end Call_Accepted;

end System.Tasking.Entry_Calls;
