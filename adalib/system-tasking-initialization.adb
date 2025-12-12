-- System.Tasking.Initialization body for Z80
-- Runtime initialization for tasking

package body System.Tasking.Initialization is

   Initialized : Boolean := False;
   Abort_Defer_Count : Natural := 0;
   Term_Handler : Termination_Handler := null;

   ------------------------
   -- Initialize_Runtime --
   ------------------------

   procedure Initialize_Runtime is
   begin
      if not Initialized then
         -- Initialize task primitives
         System.Task_Primitives.Initialize;
         Initialized := True;
      end if;
   end Initialize_Runtime;

   ----------------------
   -- Finalize_Runtime --
   ----------------------

   procedure Finalize_Runtime is
   begin
      if Initialized then
         -- Wait for all tasks to complete
         -- (Environment task does this via Complete_Master)
         Initialized := False;
      end if;
   end Finalize_Runtime;

   -------------------------
   -- Runtime_Initialized --
   -------------------------

   function Runtime_Initialized return Boolean is
   begin
      return Initialized;
   end Runtime_Initialized;

   ---------------------------------
   -- Initialize_Environment_Task --
   ---------------------------------

   procedure Initialize_Environment_Task is
   begin
      Initialize_Runtime;
      -- Environment task is task 0 (created by Initialize)
   end Initialize_Environment_Task;

   -------------------------------
   -- Finalize_Environment_Task --
   -------------------------------

   procedure Finalize_Environment_Task is
   begin
      -- Wait for all dependent tasks
      Stages.Complete_Master;
      Finalize_Runtime;
   end Finalize_Environment_Task;

   -----------------
   -- Defer_Abort --
   -----------------

   procedure Defer_Abort is
   begin
      Abort_Defer_Count := Abort_Defer_Count + 1;
   end Defer_Abort;

   -------------------
   -- Undefer_Abort --
   -------------------

   procedure Undefer_Abort is
   begin
      if Abort_Defer_Count > 0 then
         Abort_Defer_Count := Abort_Defer_Count - 1;
      end if;
   end Undefer_Abort;

   --------------------
   -- Abort_Deferred --
   --------------------

   function Abort_Deferred return Boolean is
   begin
      return Abort_Defer_Count > 0;
   end Abort_Deferred;

   -----------------------------
   -- Set_Termination_Handler --
   -----------------------------

   procedure Set_Termination_Handler (Handler : Termination_Handler) is
   begin
      Term_Handler := Handler;
   end Set_Termination_Handler;

   ------------------------------
   -- Call_Termination_Handler --
   ------------------------------

   procedure Call_Termination_Handler (T : Task_Id) is
   begin
      if Term_Handler /= null then
         Term_Handler (T);
      end if;
   end Call_Termination_Handler;

end System.Tasking.Initialization;
