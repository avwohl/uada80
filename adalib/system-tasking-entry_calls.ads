-- System.Tasking.Entry_Calls for Z80
-- Runtime support for task entry calls (rendezvous)

package System.Tasking.Entry_Calls is
   pragma Preelaborate;

   -- Maximum entries per task
   Max_Entries : constant := 8;

   -- Entry index
   type Entry_Index is range 0 .. Max_Entries;
   No_Entry : constant Entry_Index := 0;

   -- Entry call modes
   type Call_Mode is (Simple_Call, Conditional_Call, Timed_Call);

   -- Entry queue item
   type Entry_Call is record
      Caller    : Task_Id := Null_Task;
      Entry_Num : Entry_Index := No_Entry;
      Mode      : Call_Mode := Simple_Call;
      Accepted  : Boolean := False;
      Completed : Boolean := False;
   end record;

   -- Entry queue for a task
   type Entry_Queue is array (1 .. Max_Tasks) of Entry_Call;

   -- Initialize entry queue
   procedure Initialize_Entries (Queue : out Entry_Queue);

   -- Make an entry call (caller side)
   procedure Call_Entry
     (Callee    : Task_Id;
      Entry_Num : Entry_Index;
      Mode      : Call_Mode := Simple_Call);

   -- Accept an entry (callee side)
   procedure Accept_Entry
     (Entry_Num : Entry_Index;
      Caller    : out Task_Id);

   -- Complete an entry call (callee side, end of accept body)
   procedure Complete_Entry (Caller : Task_Id);

   -- Check if entry has pending calls
   function Entry_Count (T : Task_Id; Entry_Num : Entry_Index) return Natural;

   -- Cancel a pending entry call (for timed/conditional)
   procedure Cancel_Entry_Call (Caller : Task_Id);

   -- Check if call was accepted (for conditional calls)
   function Call_Accepted (Caller : Task_Id) return Boolean;

end System.Tasking.Entry_Calls;
