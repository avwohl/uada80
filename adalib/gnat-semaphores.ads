-- GNAT.Semaphores for Z80
-- Counting semaphores

package GNAT.Semaphores is
   pragma Preelaborate;

   -- Counting semaphore
   type Counting_Semaphore (Initial_Value : Natural) is limited private;

   -- Binary semaphore
   type Binary_Semaphore (Initially_Available : Boolean) is limited private;

   -- Counting semaphore operations
   procedure Seize (S : in Out Counting_Semaphore);
   procedure Release (S : in Out Counting_Semaphore);
   function Current_Count (S : Counting_Semaphore) return Natural;

   -- Binary semaphore operations
   procedure Seize (S : in Out Binary_Semaphore);
   procedure Release (S : in Out Binary_Semaphore);
   function Is_Available (S : Binary_Semaphore) return Boolean;

private

   type Counting_Semaphore (Initial_Value : Natural) is limited record
      Count : Natural := Initial_Value;
   end record;

   type Binary_Semaphore (Initially_Available : Boolean) is limited record
      Available : Boolean := Initially_Available;
   end record;

end GNAT.Semaphores;
