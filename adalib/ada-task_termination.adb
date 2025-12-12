-- Ada.Task_Termination body for Z80
-- Task termination handlers implementation

with System.Tasking;

package body Ada.Task_Termination is

   -- Per-task specific handlers (Max_Tasks = 8)
   Specific_Handlers : array (1 .. 8) of Termination_Handler :=
     (others => null);

   -- Fallback handler (per-task)
   Fallback_Handlers : array (1 .. 8) of Termination_Handler :=
     (others => null);

   --------------------------------------
   -- Set_Dependents_Fallback_Handler --
   --------------------------------------

   procedure Set_Dependents_Fallback_Handler (Handler : Termination_Handler) is
      Index : constant Natural :=
        Natural (System.Tasking.Current_Task);
   begin
      if Index in Fallback_Handlers'Range then
         Fallback_Handlers (Index) := Handler;
      end if;
   end Set_Dependents_Fallback_Handler;

   ----------------------------------
   -- Current_Task_Fallback_Handler --
   ----------------------------------

   function Current_Task_Fallback_Handler return Termination_Handler is
      Index : constant Natural :=
        Natural (System.Tasking.Current_Task);
   begin
      if Index in Fallback_Handlers'Range then
         return Fallback_Handlers (Index);
      else
         return null;
      end if;
   end Current_Task_Fallback_Handler;

   --------------------------
   -- Set_Specific_Handler --
   --------------------------

   procedure Set_Specific_Handler
     (T       : Ada.Task_Identification.Task_Id;
      Handler : Termination_Handler)
   is
      Index : constant Natural :=
        Natural (System.Tasking.To_Task_Id (T));
   begin
      if Index in Specific_Handlers'Range then
         Specific_Handlers (Index) := Handler;
      end if;
   end Set_Specific_Handler;

   ----------------------
   -- Specific_Handler --
   ----------------------

   function Specific_Handler
     (T : Ada.Task_Identification.Task_Id) return Termination_Handler
   is
      Index : constant Natural :=
        Natural (System.Tasking.To_Task_Id (T));
   begin
      if Index in Specific_Handlers'Range then
         return Specific_Handlers (Index);
      else
         return null;
      end if;
   end Specific_Handler;

end Ada.Task_Termination;
