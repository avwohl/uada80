-- System.Tasking.Rendezvous body for Z80
-- High-level rendezvous support

package body System.Tasking.Rendezvous is

   -----------------
   -- Call_Simple --
   -----------------

   procedure Call_Simple
     (Acceptor  : Task_Id;
      Entry_Num : Entry_Index)
   is
   begin
      Entry_Calls.Call_Entry (Acceptor, Entry_Num, Simple_Call);
   end Call_Simple;

   --------------
   -- Try_Call --
   --------------

   function Try_Call
     (Acceptor  : Task_Id;
      Entry_Num : Entry_Index) return Boolean
   is
      Self : constant Task_Id := Current_Task;
   begin
      Entry_Calls.Call_Entry (Acceptor, Entry_Num, Conditional_Call);

      -- Check if it was accepted
      if Entry_Calls.Call_Accepted (Self) then
         -- Wait for completion
         loop
            Enter_Protected;
            -- Check completion through Entry_Calls state
            Leave_Protected;
            exit;  -- Simplified
         end loop;
         return True;
      else
         Entry_Calls.Cancel_Entry_Call (Self);
         return False;
      end if;
   end Try_Call;

   ----------------
   -- Timed_Call --
   ----------------

   procedure Timed_Call
     (Acceptor  : Task_Id;
      Entry_Num : Entry_Index;
      Timeout   : Natural;
      Success   : out Boolean)
   is
      Self  : constant Task_Id := Current_Task;
      Count : Natural := 0;
   begin
      Entry_Calls.Call_Entry (Acceptor, Entry_Num, Timed_Call);

      -- Wait with timeout
      loop
         if Entry_Calls.Call_Accepted (Self) then
            Success := True;
            return;
         end if;

         Count := Count + 1;
         if Count >= Timeout then
            Entry_Calls.Cancel_Entry_Call (Self);
            Success := False;
            return;
         end if;

         Yield;
      end loop;
   end Timed_Call;

   --------------------
   -- Selective_Wait --
   --------------------

   procedure Selective_Wait
     (Alternatives : Accept_List;
      Selected     : out Natural;
      Caller       : out Task_Id)
   is
      Self : constant Task_Id := Current_Task;
   begin
      Selected := 0;
      Caller := Null_Task;

      -- Keep checking until an alternative is ready
      loop
         Enter_Protected;

         for I in Alternatives'Range loop
            if Alternatives (I).Guard then
               -- Check if there's a call for this entry
               if Entry_Calls.Entry_Count (Self, Alternatives (I).Entry_Num) > 0 then
                  Selected := I;
                  Leave_Protected;

                  -- Accept this entry
                  Entry_Calls.Accept_Entry (Alternatives (I).Entry_Num, Caller);
                  return;
               end if;
            end if;
         end loop;

         Leave_Protected;
         Yield;
      end loop;
   end Selective_Wait;

   ------------------
   -- Requeue_Call --
   ------------------

   procedure Requeue_Call
     (Entry_Num  : Entry_Index;
      With_Abort : Boolean := False)
   is
      pragma Unreferenced (With_Abort);
      Self : constant Task_Id := Current_Task;
   begin
      -- Requeue to same task's entry
      Requeue_Task_Entry (Self, Entry_Num, With_Abort);
   end Requeue_Call;

   ------------------------
   -- Requeue_Task_Entry --
   ------------------------

   procedure Requeue_Task_Entry
     (Acceptor   : Task_Id;
      Entry_Num  : Entry_Index;
      With_Abort : Boolean := False)
   is
      pragma Unreferenced (With_Abort);
   begin
      -- Put caller back in queue for specified entry
      -- Simplified implementation - actual requeue would preserve caller
      Entry_Calls.Call_Entry (Acceptor, Entry_Num, Simple_Call);
   end Requeue_Task_Entry;

end System.Tasking.Rendezvous;
