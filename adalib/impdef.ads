-- ImpDef package for ACATS test harness
-- Implementation-defined constants stub

with System.Storage_Elements;

package ImpDef is

   -- Annex validation flags
   Validating_Annex_C : constant Boolean := False;
   Validating_Annex_D : constant Boolean := False;
   Validating_Annex_E : constant Boolean := False;
   Validating_Annex_F : constant Boolean := False;
   Validating_Annex_G : constant Boolean := False;
   Validating_Annex_H : constant Boolean := False;

   -- Task timing constants
   Minimum_Task_Switch : constant Duration := 0.001;
   Switch_To_New_Task : constant Duration := 0.001;
   Clear_Ready_Queue : constant Duration := 0.1;
   Delay_For_Time_Past : constant Duration := 0.001;
   Time_Dependent_Reset : constant Duration := 0.001;
   Delay_Per_Random_Test : constant Duration := 0.001;

   -- Time slice procedure
   procedure Exceed_Time_Slice;

   -- Random number generator state
   Non_State_String : constant String := "Not A State";

   -- External tag value
   External_Tag_Value : constant String := "implementation_defined";

   -- Alignment constants
   Max_Default_Alignment : constant := 8;
   Max_Linker_Alignment : constant := 8;
   Maximum_Adjustment_To_Specified_Storage_Size : constant := 1024;

   -- Directory operations
   Directory_To_Create : constant String := "TESTDIR";
   Parent_Directory_Name : constant String := "..";
   Current_Directory_Name : constant String := ".";

   function Equivalent_File_Names (Left, Right : String) return Boolean;

   -- Environment variable names
   Existing_Environment_Variable_To_Read : constant String := "PATH";
   Unused_Environment_Variable_To_Modify : constant String := "ACATSVAR";

   -- External names for C interface tests
   CXB30040_External_Name : constant String := "CXB30040";
   CXB30060_External_Name : constant String := "CXB30060";
   CXB30130_External_Name : constant String := "CXB30130";
   CXB30131_External_Name : constant String := "CXB30131";
   CXB30170_External_Name : constant String := "CXB30170";
   CXB30230_External_Name : constant String := "CXB30230";
   CXB30171_Global_External_Name : constant String := "CXB30171_Global";
   CXB30171_Proc_External_Name : constant String := "CXB30171_Ada_Doubler";

end ImpDef;
