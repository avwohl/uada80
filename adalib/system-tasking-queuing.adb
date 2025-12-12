-- System.Tasking.Queuing body for Z80
-- Entry queue management

with System.Tasking.Entry_Calls;

package body System.Tasking.Queuing is

   -- Policy storage
   Policies : array (Task_Id range 1 .. Max_Tasks, 1 .. 8) of Queuing_Policy :=
     (others => (others => Default_Policy));

   ----------------
   -- Set_Policy --
   ----------------

   procedure Set_Policy
     (T         : Task_Id;
      Entry_Num : Natural;
      Policy    : Queuing_Policy)
   is
   begin
      if T in Policies'Range (1) and then Entry_Num in Policies'Range (2) then
         Policies (T, Entry_Num) := Policy;
      end if;
   end Set_Policy;

   ----------------
   -- Get_Policy --
   ----------------

   function Get_Policy
     (T         : Task_Id;
      Entry_Num : Natural) return Queuing_Policy
   is
   begin
      if T in Policies'Range (1) and then Entry_Num in Policies'Range (2) then
         return Policies (T, Entry_Num);
      else
         return Default_Policy;
      end if;
   end Get_Policy;

   ---------------
   -- Broadcast --
   ---------------

   procedure Broadcast (T : Task_Id; Entry_Num : Natural) is
      Caller : Task_Id;
   begin
      -- Accept all waiting calls for this entry
      loop
         if Entry_Calls.Entry_Count
              (T, Entry_Calls.Entry_Index (Entry_Num)) = 0
         then
            exit;
         end if;

         Entry_Calls.Accept_Entry
           (Entry_Calls.Entry_Index (Entry_Num), Caller);
         Entry_Calls.Complete_Entry (Caller);
      end loop;
   end Broadcast;

   -------------------
   -- Waiting_Count --
   -------------------

   function Waiting_Count (T : Task_Id; Entry_Num : Natural) return Natural is
   begin
      return Entry_Calls.Entry_Count (T, Entry_Calls.Entry_Index (Entry_Num));
   end Waiting_Count;

end System.Tasking.Queuing;
