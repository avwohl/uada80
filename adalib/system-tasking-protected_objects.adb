-- System.Tasking.Protected_Objects body for Z80
-- Runtime support for protected objects

package body System.Tasking.Protected_Objects is

   ---------------------------
   -- Initialize_Protection --
   ---------------------------

   procedure Initialize_Protection (Object : in Out Protection) is
   begin
      Object.Owner := Null_Task;
      Object.Lock_Count := 0;
      Object.Ceiling := Task_Priority'Last;
   end Initialize_Protection;

   -------------------------
   -- Finalize_Protection --
   -------------------------

   procedure Finalize_Protection (Object : in Out Protection) is
   begin
      -- Release any pending lock
      Object.Owner := Null_Task;
      Object.Lock_Count := 0;
   end Finalize_Protection;

   ----------
   -- Lock --
   ----------

   procedure Lock (Object : in Out Protection) is
      Self : constant Task_Id := Current_Task;
   begin
      Enter_Protected;

      -- Check for recursive lock by same task
      if Object.Owner = Self then
         Object.Lock_Count := Object.Lock_Count + 1;
         Leave_Protected;
         return;
      end if;

      -- Wait for lock to be free (with yielding)
      while Object.Owner /= Null_Task loop
         Leave_Protected;
         Yield;
         Enter_Protected;
      end loop;

      -- Acquire lock
      Object.Owner := Self;
      Object.Lock_Count := 1;

      -- Ceiling locking: raise priority if needed
      Object.Saved_Prio := Get_Task_Priority (Self);
      if Object.Ceiling > Object.Saved_Prio then
         Set_Task_Priority (Self, Object.Ceiling);
      end if;

      Leave_Protected;
   end Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Object : in Out Protection) is
      Self : constant Task_Id := Current_Task;
   begin
      Enter_Protected;

      if Object.Owner /= Self then
         -- Not the owner - error
         Leave_Protected;
         raise Program_Error;
      end if;

      Object.Lock_Count := Object.Lock_Count - 1;

      if Object.Lock_Count = 0 then
         -- Restore original priority
         Set_Task_Priority (Self, Object.Saved_Prio);
         Object.Owner := Null_Task;
      end if;

      Leave_Protected;
   end Unlock;

   --------------------
   -- Lock_Read_Only --
   --------------------

   procedure Lock_Read_Only (Object : in Out Protection) is
   begin
      -- For simplicity, read-only uses same lock as read-write
      -- A full implementation could allow multiple readers
      Lock (Object);
   end Lock_Read_Only;

   ----------------------
   -- Unlock_Read_Only --
   ----------------------

   procedure Unlock_Read_Only (Object : in Out Protection) is
   begin
      Unlock (Object);
   end Unlock_Read_Only;

   ---------------
   -- Get_Owner --
   ---------------

   function Get_Owner (Object : Protection) return Task_Id is
   begin
      return Object.Owner;
   end Get_Owner;

   ---------------
   -- Is_Locked --
   ---------------

   function Is_Locked (Object : Protection) return Boolean is
   begin
      return Object.Owner /= Null_Task;
   end Is_Locked;

end System.Tasking.Protected_Objects;
