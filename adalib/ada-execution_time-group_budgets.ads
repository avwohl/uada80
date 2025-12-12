-- Ada.Execution_Time.Group_Budgets for Z80
-- Group CPU budgets (Ada 2012)

with Ada.Task_Identification;
with Ada.Real_Time;

package Ada.Execution_Time.Group_Budgets is
   pragma Preelaborate;

   type Group_Budget is tagged limited private;

   type Group_Budget_Handler is access protected procedure
     (GB : in Out Group_Budget);

   Min_Handler_Ceiling : constant System.Any_Priority :=
     System.Interrupt_Priority'Last;

   Group_Budget_Error : exception;

   -- Add/Remove tasks from group
   procedure Add_Task
     (GB : in Out Group_Budget;
      T  : Ada.Task_Identification.Task_Id);

   procedure Remove_Task
     (GB : in Out Group_Budget;
      T  : Ada.Task_Identification.Task_Id);

   function Is_Member
     (GB : Group_Budget;
      T  : Ada.Task_Identification.Task_Id) return Boolean;

   function Is_A_Group_Member
     (T : Ada.Task_Identification.Task_Id) return Boolean;

   function Members (GB : Group_Budget) return
     Ada.Task_Identification.Task_Array;

   -- Budget management
   procedure Replenish
     (GB : in Out Group_Budget;
      To : Ada.Real_Time.Time_Span);

   procedure Add
     (GB       : in Out Group_Budget;
      Interval : Ada.Real_Time.Time_Span);

   function Budget_Has_Expired (GB : Group_Budget) return Boolean;

   function Budget_Remaining (GB : Group_Budget)
     return Ada.Real_Time.Time_Span;

   -- Handler management
   procedure Set_Handler
     (GB      : in Out Group_Budget;
      Handler : Group_Budget_Handler);

   function Current_Handler (GB : Group_Budget)
     return Group_Budget_Handler;

   procedure Cancel_Handler
     (GB        : in Out Group_Budget;
      Cancelled : out Boolean);

private

   Max_Group_Members : constant := 8;

   type Group_Budget is tagged limited record
      Members : array (1 .. Max_Group_Members) of
        Ada.Task_Identification.Task_Id :=
          (others => Ada.Task_Identification.Null_Task_Id);
      Member_Count : Natural := 0;
      Budget       : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero;
      Handler      : Group_Budget_Handler := null;
   end record;

end Ada.Execution_Time.Group_Budgets;
