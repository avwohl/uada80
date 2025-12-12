-- System.Tasking.Queuing for Z80
-- Entry queue management

package System.Tasking.Queuing is
   pragma Preelaborate;

   -- Queue policies
   type Queuing_Policy is (FIFO_Queuing, Priority_Queuing);

   -- Default policy
   Default_Policy : constant Queuing_Policy := FIFO_Queuing;

   -- Set queuing policy for an entry
   procedure Set_Policy
     (T         : Task_Id;
      Entry_Num : Natural;
      Policy    : Queuing_Policy);

   -- Get queuing policy for an entry
   function Get_Policy
     (T         : Task_Id;
      Entry_Num : Natural) return Queuing_Policy;

   -- Broadcast to all waiting tasks (for barriers)
   procedure Broadcast (T : Task_Id; Entry_Num : Natural);

   -- Count waiters
   function Waiting_Count (T : Task_Id; Entry_Num : Natural) return Natural;

end System.Tasking.Queuing;
