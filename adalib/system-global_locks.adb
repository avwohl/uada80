-- System.Global_Locks body for Z80
-- Global lock implementation

with System.Machine_Code;

package body System.Global_Locks is

   -----------------
   -- Create_Lock --
   -----------------

   procedure Create_Lock (L : out Lock_Type; Level : Integer := 0) is
   begin
      L.Locked := False;
      L.Level := Level;
      L.Owner := 0;
   end Create_Lock;

   ------------------
   -- Acquire_Lock --
   ------------------

   procedure Acquire_Lock (L : in Out Lock_Type) is
   begin
      -- Spin until lock acquired
      loop
         System.Machine_Code.Asm ("di", Volatile => True);

         if not L.Locked then
            L.Locked := True;
            L.Owner := 1;  -- Current task (simplified)
            System.Machine_Code.Asm ("ei", Volatile => True);
            return;
         end if;

         System.Machine_Code.Asm ("ei", Volatile => True);

         -- Brief yield (HALT waits for interrupt)
         System.Machine_Code.Asm ("halt", Volatile => True);
      end loop;
   end Acquire_Lock;

   --------------
   -- Try_Lock --
   --------------

   function Try_Lock (L : in Out Lock_Type) return Boolean is
      Result : Boolean;
   begin
      System.Machine_Code.Asm ("di", Volatile => True);

      if not L.Locked then
         L.Locked := True;
         L.Owner := 1;
         Result := True;
      else
         Result := False;
      end if;

      System.Machine_Code.Asm ("ei", Volatile => True);
      return Result;
   end Try_Lock;

   ------------------
   -- Release_Lock --
   ------------------

   procedure Release_Lock (L : in Out Lock_Type) is
   begin
      System.Machine_Code.Asm ("di", Volatile => True);
      L.Locked := False;
      L.Owner := 0;
      System.Machine_Code.Asm ("ei", Volatile => True);
   end Release_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : in Out Lock_Type) is
   begin
      L.Locked := False;
      L.Owner := 0;
   end Finalize_Lock;

end System.Global_Locks;
