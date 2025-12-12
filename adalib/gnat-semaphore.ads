-- GNAT.Semaphore for Z80
-- Counting semaphore implementation

package GNAT.Semaphore is
   pragma Preelaborate;

   type Counting_Semaphore (Initial_Value : Natural) is limited private;
   --  Counting semaphore with initial value

   procedure Seize (Sem : in Out Counting_Semaphore);
   --  Wait/P operation (decrement)

   procedure Release (Sem : in Out Counting_Semaphore);
   --  Signal/V operation (increment)

   function Current_Count (Sem : Counting_Semaphore) return Natural;
   --  Return current count

   type Binary_Semaphore is limited private;
   --  Binary semaphore (mutex)

   procedure Seize (Sem : in Out Binary_Semaphore);
   --  Acquire the semaphore

   procedure Release (Sem : in Out Binary_Semaphore);
   --  Release the semaphore

   function Is_Seized (Sem : Binary_Semaphore) return Boolean;
   --  Check if semaphore is seized

private

   type Counting_Semaphore (Initial_Value : Natural) is record
      Count : Natural := Initial_Value;
   end record;

   type Binary_Semaphore is record
      Seized : Boolean := False;
   end record;

end GNAT.Semaphore;
