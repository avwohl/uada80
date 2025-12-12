-- System.Multiprocessors.Dispatching_Domains body for Z80
-- Dispatching domain support implementation

package body System.Multiprocessors.Dispatching_Domains is

   ------------
   -- Create --
   ------------

   function Create (First, Last : CPU) return Dispatching_Domain is
   begin
      if First > Last or First < 1 or Last > 1 then
         raise Dispatching_Domain_Error;
      end if;
      return (First_CPU => First, Last_CPU => Last);
   end Create;

   -------------------
   -- Get_First_CPU --
   -------------------

   function Get_First_CPU (Domain : Dispatching_Domain) return CPU is
   begin
      return Domain.First_CPU;
   end Get_First_CPU;

   ------------------
   -- Get_Last_CPU --
   ------------------

   function Get_Last_CPU (Domain : Dispatching_Domain) return CPU is
   begin
      return Domain.Last_CPU;
   end Get_Last_CPU;

   ----------------------------
   -- Get_Dispatching_Domain --
   ----------------------------

   function Get_Dispatching_Domain return Dispatching_Domain is
   begin
      return System_Dispatching_Domain;
   end Get_Dispatching_Domain;

   -----------------
   -- Assign_Task --
   -----------------

   procedure Assign_Task
     (Domain : in Out Dispatching_Domain;
      CPU    : System.Multiprocessors.CPU := Not_A_Specific_CPU)
   is
      pragma Unreferenced (Domain, CPU);
   begin
      -- Z80 has only one CPU, nothing to do
      null;
   end Assign_Task;

   -------------
   -- Set_CPU --
   -------------

   procedure Set_CPU (CPU : System.Multiprocessors.CPU) is
      pragma Unreferenced (CPU);
   begin
      -- Z80 has only one CPU, nothing to do
      null;
   end Set_CPU;

   -------------
   -- Get_CPU --
   -------------

   function Get_CPU return System.Multiprocessors.CPU is
   begin
      return 1;
   end Get_CPU;

   -----------------------------
   -- Delay_Until_And_Set_CPU --
   -----------------------------

   procedure Delay_Until_And_Set_CPU
     (Delay_Until_Time : not null access function return Duration;
      CPU              : System.Multiprocessors.CPU)
   is
      pragma Unreferenced (CPU);
      Wait_Time : constant Duration := Delay_Until_Time.all;
      pragma Unreferenced (Wait_Time);
   begin
      -- Simplified: just return, actual delay would need timer support
      null;
   end Delay_Until_And_Set_CPU;

end System.Multiprocessors.Dispatching_Domains;
