-- System.Parameters for Z80
-- System configuration parameters

package System.Parameters is
   pragma Pure;

   -- Stack size parameters
   Default_Stack_Size : constant := 256;
   Minimum_Stack_Size : constant := 64;
   Default_Sec_Stack_Size : constant := 128;

   -- Maximum tasks supported
   Max_Tasks : constant := 8;

   -- Priority range
   Default_Priority : constant := 8;
   Max_Priority     : constant := 15;
   Min_Priority     : constant := 0;

   -- Time parameters
   Tick_Duration : constant Duration := 0.02;  -- 20ms tick

   -- Memory allocation
   Default_Heap_Size : constant := 8192;  -- 8KB default heap

   -- Single processor system
   Single_Lock : constant Boolean := True;

   -- Runtime checks
   Stack_Check_Probes : constant Boolean := True;
   Stack_Check_Limit  : constant Boolean := True;

   -- Tasking model
   type Tasking_Runtime_Type is (No_Tasking, Single_Task, Multi_Task);
   Tasking_Runtime : constant Tasking_Runtime_Type := Multi_Task;

end System.Parameters;
