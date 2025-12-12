-- System.Tasking.Rendezvous for Z80
-- High-level rendezvous support

with System.Tasking.Entry_Calls;

package System.Tasking.Rendezvous is
   pragma Preelaborate;

   use System.Tasking.Entry_Calls;

   -- Simple entry call (blocking)
   procedure Call_Simple
     (Acceptor  : Task_Id;
      Entry_Num : Entry_Index);

   -- Conditional entry call (non-blocking)
   function Try_Call
     (Acceptor  : Task_Id;
      Entry_Num : Entry_Index) return Boolean;

   -- Timed entry call
   procedure Timed_Call
     (Acceptor  : Task_Id;
      Entry_Num : Entry_Index;
      Timeout   : Natural;
      Success   : out Boolean);

   -- Selective accept support
   type Accept_Alternative is record
      Entry_Num : Entry_Index;
      Guard     : Boolean;
   end record;

   type Accept_List is array (Positive range <>) of Accept_Alternative;

   -- Select with open alternatives
   procedure Selective_Wait
     (Alternatives : Accept_List;
      Selected     : out Natural;
      Caller       : out Task_Id);

   -- Requeue support
   procedure Requeue_Call
     (Entry_Num  : Entry_Index;
      With_Abort : Boolean := False);

   procedure Requeue_Task_Entry
     (Acceptor   : Task_Id;
      Entry_Num  : Entry_Index;
      With_Abort : Boolean := False);

end System.Tasking.Rendezvous;
