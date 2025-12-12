-- System.Global_Locks for Z80
-- Global lock primitives for inter-task synchronization

package System.Global_Locks is
   pragma Preelaborate;

   -- Lock type
   type Lock_Type is private;

   -- Create a lock
   procedure Create_Lock (L : out Lock_Type; Level : Integer := 0);

   -- Acquire lock (blocking)
   procedure Acquire_Lock (L : in Out Lock_Type);

   -- Try to acquire lock (non-blocking)
   function Try_Lock (L : in Out Lock_Type) return Boolean;

   -- Release lock
   procedure Release_Lock (L : in Out Lock_Type);

   -- Finalize lock
   procedure Finalize_Lock (L : in Out Lock_Type);

private
   type Lock_Type is record
      Locked  : Boolean := False;
      Level   : Integer := 0;
      Owner   : Natural := 0;  -- Task ID
   end record;

end System.Global_Locks;
