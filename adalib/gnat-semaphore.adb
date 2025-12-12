-- GNAT.Semaphore body for Z80
-- Counting semaphore implementation

with System.Machine_Code;
use System.Machine_Code;

package body GNAT.Semaphore is

   -- Disable interrupts for atomic operations on Z80
   procedure Disable_Interrupts is
   begin
      Asm ("di", Volatile => True);
   end Disable_Interrupts;

   procedure Enable_Interrupts is
   begin
      Asm ("ei", Volatile => True);
   end Enable_Interrupts;

   -----------
   -- Seize --
   -----------

   procedure Seize (Sem : in Out Counting_Semaphore) is
   begin
      -- Busy wait if count is zero
      loop
         Disable_Interrupts;
         if Sem.Count > 0 then
            Sem.Count := Sem.Count - 1;
            Enable_Interrupts;
            exit;
         end if;
         Enable_Interrupts;
         -- Small delay to prevent tight spinning
         null;
      end loop;
   end Seize;

   -------------
   -- Release --
   -------------

   procedure Release (Sem : in Out Counting_Semaphore) is
   begin
      Disable_Interrupts;
      Sem.Count := Sem.Count + 1;
      Enable_Interrupts;
   end Release;

   -------------------
   -- Current_Count --
   -------------------

   function Current_Count (Sem : Counting_Semaphore) return Natural is
   begin
      return Sem.Count;
   end Current_Count;

   -----------
   -- Seize --
   -----------

   procedure Seize (Sem : in Out Binary_Semaphore) is
   begin
      loop
         Disable_Interrupts;
         if not Sem.Seized then
            Sem.Seized := True;
            Enable_Interrupts;
            exit;
         end if;
         Enable_Interrupts;
         null;
      end loop;
   end Seize;

   -------------
   -- Release --
   -------------

   procedure Release (Sem : in Out Binary_Semaphore) is
   begin
      Disable_Interrupts;
      Sem.Seized := False;
      Enable_Interrupts;
   end Release;

   ---------------
   -- Is_Seized --
   ---------------

   function Is_Seized (Sem : Binary_Semaphore) return Boolean is
   begin
      return Sem.Seized;
   end Is_Seized;

end GNAT.Semaphore;
