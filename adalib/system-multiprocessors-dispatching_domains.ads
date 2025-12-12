-- System.Multiprocessors.Dispatching_Domains for Z80
-- Dispatching domain support for multiprocessors
--
-- Note: Z80 is single-processor, so dispatching domains are trivial.

with System.Multiprocessors;

package System.Multiprocessors.Dispatching_Domains is
   pragma Preelaborate;

   Dispatching_Domain_Error : exception;

   type Dispatching_Domain is private;

   System_Dispatching_Domain : constant Dispatching_Domain;

   function Create (First, Last : CPU) return Dispatching_Domain;
   -- Creates a new dispatching domain

   function Get_First_CPU (Domain : Dispatching_Domain) return CPU;
   -- Returns the first CPU in the domain

   function Get_Last_CPU (Domain : Dispatching_Domain) return CPU;
   -- Returns the last CPU in the domain

   function Get_Dispatching_Domain return Dispatching_Domain;
   -- Returns the dispatching domain of the calling task

   procedure Assign_Task
     (Domain : in Out Dispatching_Domain;
      CPU    : System.Multiprocessors.CPU := Not_A_Specific_CPU);
   -- Assigns the calling task to the specified domain and CPU

   procedure Set_CPU (CPU : System.Multiprocessors.CPU);
   -- Sets the CPU affinity for the calling task

   function Get_CPU return System.Multiprocessors.CPU;
   -- Gets the CPU on which the calling task is executing

   procedure Delay_Until_And_Set_CPU
     (Delay_Until_Time : not null access function return Duration;
      CPU              : System.Multiprocessors.CPU);
   -- Delays and sets CPU atomically

private
   type Dispatching_Domain is record
      First_CPU : CPU := 1;
      Last_CPU  : CPU := 1;
   end record;

   System_Dispatching_Domain : constant Dispatching_Domain :=
     (First_CPU => 1, Last_CPU => 1);

end System.Multiprocessors.Dispatching_Domains;
