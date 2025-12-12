-- System.Tasking.Protected_Objects for Z80
-- Runtime support for protected objects
--
-- Protected objects use interrupt disable/enable for mutual exclusion

package System.Tasking.Protected_Objects is
   pragma Preelaborate;

   -- Protected object runtime data
   type Protection is limited private;

   -- Initialize a protected object
   procedure Initialize_Protection (Object : in out Protection);

   -- Finalize a protected object
   procedure Finalize_Protection (Object : in Out Protection);

   -- Lock for protected procedure/function entry
   procedure Lock (Object : in Out Protection);

   -- Unlock after protected procedure/function
   procedure Unlock (Object : in Out Protection);

   -- Lock for read-only access (protected function)
   procedure Lock_Read_Only (Object : in Out Protection);

   -- Unlock after read-only access
   procedure Unlock_Read_Only (Object : in Out Protection);

   -- Get current owner (for debugging)
   function Get_Owner (Object : Protection) return Task_Id;

   -- Check if object is locked
   function Is_Locked (Object : Protection) return Boolean;

private
   type Protection is record
      Owner       : Task_Id := Null_Task;
      Lock_Count  : Natural := 0;
      Ceiling     : Task_Priority := Task_Priority'Last;
      Saved_Prio  : Task_Priority := Default_Priority;
   end record;

end System.Tasking.Protected_Objects;
