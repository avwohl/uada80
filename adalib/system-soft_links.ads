-- System.Soft_Links for Z80
-- Runtime soft links for single-task Z80 environment

package System.Soft_Links is
   pragma Preelaborate;

   -- Get current task ID (always 0 for single-task Z80)
   function Get_Current_Task return Natural;

   -- Lock/Unlock operations (no-ops for single-task)
   procedure Lock_Task;
   procedure Unlock_Task;

   -- Abort operations (no-ops for single-task)
   procedure Abort_Defer;
   procedure Abort_Undefer;

   -- Check for abort (always False for single-task)
   function Check_Abort return Boolean;

   -- Get secondary stack pointer
   function Get_Sec_Stack return System.Address;

   -- Set secondary stack pointer
   procedure Set_Sec_Stack (Addr : System.Address);

   -- Get GNAT exception occurrence
   function Get_GNAT_Exception return System.Address;

   -- Set GNAT exception occurrence
   procedure Set_GNAT_Exception (X : System.Address);

private
   -- Secondary stack pointer for the single task
   Sec_Stack_Ptr : System.Address := System.Null_Address;

   -- Current exception pointer
   Current_Exception : System.Address := System.Null_Address;

end System.Soft_Links;
