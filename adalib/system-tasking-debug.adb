-- System.Tasking.Debug body for Z80
-- Tasking debug and diagnostic support

with System.Task_Primitives;

package body System.Tasking.Debug is

   Switch_Count : Natural := 0;
   Tick_Count   : Natural := 0;

   -------------------
   -- Get_All_Tasks --
   -------------------

   procedure Get_All_Tasks
     (Info  : out array (Task_Id range <>) of Task_Info;
      Count : out Natural)
   is
   begin
      Count := 0;
      for T in Task_Id'First .. Max_Tasks loop
         if Stages.Get_Stage (T) /= Stages.Not_Created then
            if Count < Info'Length then
               Count := Count + 1;
               Info (Info'First + Task_Id (Count) - 1) :=
                 (Id       => T,
                  Priority => System.Task_Primitives.Get_Priority (T),
                  State    => Get_State (T),
                  Stage    => Stages.Get_Stage (T));
            end if;
         end if;
      end loop;
   end Get_All_Tasks;

   -------------------
   -- Get_Task_Info --
   -------------------

   function Get_Task_Info (T : Task_Id) return Task_Info is
   begin
      return (Id       => T,
              Priority => System.Task_Primitives.Get_Priority (T),
              State    => Get_State (T),
              Stage    => Stages.Get_Stage (T));
   end Get_Task_Info;

   ----------------------
   -- Print_Task_State --
   ----------------------

   procedure Print_Task_State (T : Task_Id) is
      pragma Unreferenced (T);
   begin
      -- Debug output not available on Z80 without console
      null;
   end Print_Task_State;

   ---------------------
   -- Print_All_Tasks --
   ---------------------

   procedure Print_All_Tasks is
   begin
      for T in Task_Id'First .. Max_Tasks loop
         if Stages.Get_Stage (T) /= Stages.Not_Created then
            Print_Task_State (T);
         end if;
      end loop;
   end Print_All_Tasks;

   --------------------------
   -- Context_Switch_Count --
   --------------------------

   function Context_Switch_Count return Natural is
   begin
      return Switch_Count;
   end Context_Switch_Count;

   ----------------------
   -- Timer_Tick_Count --
   ----------------------

   function Timer_Tick_Count return Natural is
   begin
      return Tick_Count;
   end Timer_Tick_Count;

   ---------------------
   -- Task_Breakpoint --
   ---------------------

   procedure Task_Breakpoint (T : Task_Id) is
      pragma Unreferenced (T);
   begin
      -- Could trigger RST 38h for debugger if present
      null;
   end Task_Breakpoint;

   ------------------
   -- Enable_Trace --
   ------------------

   procedure Enable_Trace is
   begin
      Trace_Enabled := True;
   end Enable_Trace;

   -------------------
   -- Disable_Trace --
   -------------------

   procedure Disable_Trace is
   begin
      Trace_Enabled := False;
   end Disable_Trace;

   -- Internal: increment counters (called by scheduler)
   procedure Increment_Switch_Count is
   begin
      Switch_Count := Switch_Count + 1;
   end Increment_Switch_Count;

   procedure Increment_Tick_Count is
   begin
      Tick_Count := Tick_Count + 1;
   end Increment_Tick_Count;

end System.Tasking.Debug;
