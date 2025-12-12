-- GNAT.Semaphores body for Z80
-- Counting semaphores implementation

with System.Tasking;

package body GNAT.Semaphores is

   -- Counting Semaphore

   -----------
   -- Seize --
   -----------

   procedure Seize (S : in Out Counting_Semaphore) is
   begin
      -- Wait until count > 0
      loop
         System.Tasking.Enter_Protected;
         if S.Count > 0 then
            S.Count := S.Count - 1;
            System.Tasking.Leave_Protected;
            return;
         end if;
         System.Tasking.Leave_Protected;
         System.Tasking.Yield;
      end loop;
   end Seize;

   -------------
   -- Release --
   -------------

   procedure Release (S : in Out Counting_Semaphore) is
   begin
      System.Tasking.Enter_Protected;
      S.Count := S.Count + 1;
      System.Tasking.Leave_Protected;
   end Release;

   -------------------
   -- Current_Count --
   -------------------

   function Current_Count (S : Counting_Semaphore) return Natural is
   begin
      return S.Count;
   end Current_Count;

   -- Binary Semaphore

   -----------
   -- Seize --
   -----------

   procedure Seize (S : in Out Binary_Semaphore) is
   begin
      -- Wait until available
      loop
         System.Tasking.Enter_Protected;
         if S.Available then
            S.Available := False;
            System.Tasking.Leave_Protected;
            return;
         end if;
         System.Tasking.Leave_Protected;
         System.Tasking.Yield;
      end loop;
   end Seize;

   -------------
   -- Release --
   -------------

   procedure Release (S : in Out Binary_Semaphore) is
   begin
      System.Tasking.Enter_Protected;
      S.Available := True;
      System.Tasking.Leave_Protected;
   end Release;

   ------------------
   -- Is_Available --
   ------------------

   function Is_Available (S : Binary_Semaphore) return Boolean is
   begin
      return S.Available;
   end Is_Available;

end GNAT.Semaphores;
