-- System.Soft_Links body for Z80
-- Runtime soft links for single-task Z80 environment

package body System.Soft_Links is

   ----------------------
   -- Get_Current_Task --
   ----------------------

   function Get_Current_Task return Natural is
   begin
      -- Single task system, always task 0
      return 0;
   end Get_Current_Task;

   ---------------
   -- Lock_Task --
   ---------------

   procedure Lock_Task is
   begin
      -- No-op for single-task system
      null;
   end Lock_Task;

   -----------------
   -- Unlock_Task --
   -----------------

   procedure Unlock_Task is
   begin
      -- No-op for single-task system
      null;
   end Unlock_Task;

   -----------------
   -- Abort_Defer --
   -----------------

   procedure Abort_Defer is
   begin
      -- No-op for single-task system
      null;
   end Abort_Defer;

   -------------------
   -- Abort_Undefer --
   -------------------

   procedure Abort_Undefer is
   begin
      -- No-op for single-task system
      null;
   end Abort_Undefer;

   -----------------
   -- Check_Abort --
   -----------------

   function Check_Abort return Boolean is
   begin
      -- No abort in single-task system
      return False;
   end Check_Abort;

   -------------------
   -- Get_Sec_Stack --
   -------------------

   function Get_Sec_Stack return System.Address is
   begin
      return Sec_Stack_Ptr;
   end Get_Sec_Stack;

   -------------------
   -- Set_Sec_Stack --
   -------------------

   procedure Set_Sec_Stack (Addr : System.Address) is
   begin
      Sec_Stack_Ptr := Addr;
   end Set_Sec_Stack;

   ------------------------
   -- Get_GNAT_Exception --
   ------------------------

   function Get_GNAT_Exception return System.Address is
   begin
      return Current_Exception;
   end Get_GNAT_Exception;

   ------------------------
   -- Set_GNAT_Exception --
   ------------------------

   procedure Set_GNAT_Exception (X : System.Address) is
   begin
      Current_Exception := X;
   end Set_GNAT_Exception;

end System.Soft_Links;
