-- Ada.Execution_Time.Group_Budgets body for Z80
-- Group CPU budgets implementation

package body Ada.Execution_Time.Group_Budgets is

   use Ada.Task_Identification;

   --------------
   -- Add_Task --
   --------------

   procedure Add_Task
     (GB : in Out Group_Budget;
      T  : Task_Id)
   is
   begin
      if GB.Member_Count >= Max_Group_Members then
         raise Group_Budget_Error with "Group full";
      end if;

      -- Check if already member
      for I in 1 .. GB.Member_Count loop
         if GB.Members (I) = T then
            return;  -- Already a member
         end if;
      end loop;

      GB.Member_Count := GB.Member_Count + 1;
      GB.Members (GB.Member_Count) := T;
   end Add_Task;

   -----------------
   -- Remove_Task --
   -----------------

   procedure Remove_Task
     (GB : in Out Group_Budget;
      T  : Task_Id)
   is
   begin
      for I in 1 .. GB.Member_Count loop
         if GB.Members (I) = T then
            -- Shift remaining members
            for J in I .. GB.Member_Count - 1 loop
               GB.Members (J) := GB.Members (J + 1);
            end loop;
            GB.Members (GB.Member_Count) := Null_Task_Id;
            GB.Member_Count := GB.Member_Count - 1;
            return;
         end if;
      end loop;
   end Remove_Task;

   ---------------
   -- Is_Member --
   ---------------

   function Is_Member
     (GB : Group_Budget;
      T  : Task_Id) return Boolean
   is
   begin
      for I in 1 .. GB.Member_Count loop
         if GB.Members (I) = T then
            return True;
         end if;
      end loop;
      return False;
   end Is_Member;

   -----------------------
   -- Is_A_Group_Member --
   -----------------------

   function Is_A_Group_Member
     (T : Task_Id) return Boolean
   is
      pragma Unreferenced (T);
   begin
      -- Would need global group tracking
      return False;
   end Is_A_Group_Member;

   -------------
   -- Members --
   -------------

   function Members (GB : Group_Budget) return Task_Array is
      Result : Task_Array (1 .. GB.Member_Count);
   begin
      for I in Result'Range loop
         Result (I) := GB.Members (I);
      end loop;
      return Result;
   end Members;

   ---------------
   -- Replenish --
   ---------------

   procedure Replenish
     (GB : in Out Group_Budget;
      To : Ada.Real_Time.Time_Span)
   is
   begin
      GB.Budget := To;
   end Replenish;

   ---------
   -- Add --
   ---------

   procedure Add
     (GB       : in Out Group_Budget;
      Interval : Ada.Real_Time.Time_Span)
   is
      use type Ada.Real_Time.Time_Span;
   begin
      GB.Budget := GB.Budget + Interval;
   end Add;

   ------------------------
   -- Budget_Has_Expired --
   ------------------------

   function Budget_Has_Expired (GB : Group_Budget) return Boolean is
      use type Ada.Real_Time.Time_Span;
   begin
      return GB.Budget <= Ada.Real_Time.Time_Span_Zero;
   end Budget_Has_Expired;

   ----------------------
   -- Budget_Remaining --
   ----------------------

   function Budget_Remaining (GB : Group_Budget)
     return Ada.Real_Time.Time_Span
   is
   begin
      return GB.Budget;
   end Budget_Remaining;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (GB      : in Out Group_Budget;
      Handler : Group_Budget_Handler)
   is
   begin
      GB.Handler := Handler;
   end Set_Handler;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler (GB : Group_Budget)
     return Group_Budget_Handler
   is
   begin
      return GB.Handler;
   end Current_Handler;

   --------------------
   -- Cancel_Handler --
   --------------------

   procedure Cancel_Handler
     (GB        : in Out Group_Budget;
      Cancelled : out Boolean)
   is
   begin
      Cancelled := GB.Handler /= null;
      GB.Handler := null;
   end Cancel_Handler;

end Ada.Execution_Time.Group_Budgets;
