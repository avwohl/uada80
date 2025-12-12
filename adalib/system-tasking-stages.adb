-- System.Tasking.Stages body for Z80
-- Task lifecycle stages

with System.Task_Primitives;

package body System.Tasking.Stages is

   -- Stack allocation area
   Stack_Pool : array (1 .. 4096) of Character;
   pragma Volatile (Stack_Pool);

   Stack_Offset : Natural := 0;

   -- Stage tracking
   Stages : array (Task_Id range 1 .. Max_Tasks) of Activation_Stage :=
     (others => Not_Created);

   -- Master level tracking
   Current_Master : Natural := 0;

   -----------------
   -- Create_Task --
   -----------------

   procedure Create_Task
     (Task_Image  : String;
      Entry_Point : System.Address;
      Stack_Size  : Natural;
      Priority    : Task_Priority;
      Task_Ptr    : out Task_Id)
   is
      pragma Unreferenced (Task_Image);
      Stack_Base : System.Address;
      Actual_Size : Natural := Stack_Size;
   begin
      -- Allocate stack from pool
      if Actual_Size < 256 then
         Actual_Size := 256;  -- Minimum stack
      end if;

      if Stack_Offset + Actual_Size > Stack_Pool'Length then
         raise Storage_Error;
      end if;

      Stack_Base := Stack_Pool (Stack_Offset + 1)'Address;
      Stack_Offset := Stack_Offset + Actual_Size;

      -- Create via primitives
      System.Task_Primitives.Create_Task
        (Id          => Task_Ptr,
         Entry_Point => Entry_Point,
         Stack_Base  => Stack_Base,
         Stack_Size  => Actual_Size,
         Priority    => Priority);

      if Task_Ptr /= Null_Task then
         Stages (Task_Ptr) := Created;
      end if;
   end Create_Task;

   --------------------
   -- Activate_Tasks --
   --------------------

   procedure Activate_Tasks (Chain : Task_Id) is
   begin
      if Chain = Null_Task then
         return;
      end if;

      Stages (Chain) := Activating;
      System.Task_Primitives.Start_Task (Chain);
      Stages (Chain) := Active;
   end Activate_Tasks;

   -------------------------
   -- Complete_Activation --
   -------------------------

   procedure Complete_Activation is
      Self : constant Task_Id := Current_Task;
   begin
      if Self /= Null_Task then
         Stages (Self) := Active;
      end if;
   end Complete_Activation;

   -------------------
   -- Complete_Task --
   -------------------

   procedure Complete_Task is
      Self : constant Task_Id := Current_Task;
   begin
      if Self /= Null_Task then
         Stages (Self) := Complete;
         System.Task_Primitives.Terminate_Self;
         Stages (Self) := Terminated;
      end if;
   end Complete_Task;

   -----------------
   -- Abort_Tasks --
   -----------------

   procedure Abort_Tasks (Chain : Task_Id) is
   begin
      if Chain = Null_Task then
         return;
      end if;

      -- Suspend the task (simplified abort)
      System.Task_Primitives.Suspend (Chain);
      Stages (Chain) := Terminated;
   end Abort_Tasks;

   --------------------------
   -- Wait_For_Dependents --
   --------------------------

   procedure Wait_For_Dependents is
   begin
      -- Wait for all tasks at current master level to complete
      loop
         declare
            All_Done : Boolean := True;
         begin
            for I in Stages'Range loop
               if Stages (I) = Active or else Stages (I) = Activating then
                  All_Done := False;
                  exit;
               end if;
            end loop;

            if All_Done then
               exit;
            end if;
         end;

         Yield;
      end loop;
   end Wait_For_Dependents;

   ---------------
   -- Get_Stage --
   ---------------

   function Get_Stage (T : Task_Id) return Activation_Stage is
   begin
      if T = Null_Task or else T > Max_Tasks then
         return Not_Created;
      end if;
      return Stages (T);
   end Get_Stage;

   ------------------
   -- Enter_Master --
   ------------------

   procedure Enter_Master is
   begin
      Current_Master := Current_Master + 1;
   end Enter_Master;

   ---------------------
   -- Complete_Master --
   ---------------------

   procedure Complete_Master is
   begin
      Wait_For_Dependents;
      if Current_Master > 0 then
         Current_Master := Current_Master - 1;
      end if;
   end Complete_Master;

   ------------------
   -- Master_Level --
   ------------------

   function Master_Level return Natural is
   begin
      return Current_Master;
   end Master_Level;

end System.Tasking.Stages;
