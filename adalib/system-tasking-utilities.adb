-- System.Tasking.Utilities body for Z80
-- Tasking utilities implementation

package body System.Tasking.Utilities is

   ----------------------
   -- Get_Current_Task --
   ----------------------

   function Get_Current_Task return Task_Id is
   begin
      return Current_Task;
   end Get_Current_Task;

   -------------------
   -- Get_Task_Name --
   -------------------

   function Get_Task_Name (T : Task_Id) return String is
   begin
      if T = Null_Task then
         return "";
      end if;
      return "Task_" & Integer'Image (Integer (T));
   end Get_Task_Name;

   -----------------------
   -- Get_Task_Priority --
   -----------------------

   function Get_Task_Priority (T : Task_Id) return Integer is
   begin
      if T = Null_Task then
         return 0;
      end if;
      return T.Priority;
   end Get_Task_Priority;

   -----------------------
   -- Set_Task_Priority --
   -----------------------

   procedure Set_Task_Priority
     (T : Task_Id;
      Priority : Integer)
   is
   begin
      if T /= Null_Task then
         T.Priority := Priority;
      end if;
   end Set_Task_Priority;

   --------------------
   -- Get_Task_State --
   --------------------

   function Get_Task_State (T : Task_Id) return Task_State is
   begin
      if T = Null_Task then
         return Terminated;
      end if;
      return T.State;
   end Get_Task_State;

   -----------
   -- Yield --
   -----------

   procedure Yield is
   begin
      -- Trigger a task switch by waiting briefly
      null;  -- On Z80, this could trigger interrupt check
   end Yield;

   -----------
   -- Sleep --
   -----------

   procedure Sleep (Duration_MS : Natural) is
      pragma Unreferenced (Duration_MS);
   begin
      -- Simple delay loop
      Yield;
   end Sleep;

end System.Tasking.Utilities;
