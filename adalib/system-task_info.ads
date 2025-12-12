-- System.Task_Info for Z80
-- Task information (stub for single-task environment)

package System.Task_Info is
   pragma Preelaborate;

   -- Task info type (minimal for single-task Z80)
   type Task_Info_Type is new Natural range 0 .. 0;

   -- Default task info
   Unspecified_Task_Info : constant Task_Info_Type := 0;

   -- Number of processors (always 1 for Z80)
   Number_Of_Processors : constant := 1;

end System.Task_Info;
