-- Ada.Dispatching_Domains body for Z80
-- Multiprocessor dispatching domains implementation

package body Ada.Dispatching_Domains is

   -- Z80 is single CPU, simplified implementation

   Current_CPU : CPU_Range := 1;

   ------------
   -- Create --
   ------------

   function Create (First, Last : CPU) return Dispatching_Domain is
      pragma Unreferenced (First, Last);
   begin
      -- Single CPU, can only have one domain
      return System_Dispatching_Domain;
   end Create;

   -------------------
   -- Get_First_CPU --
   -------------------

   function Get_First_CPU (Domain : Dispatching_Domain) return CPU is
      pragma Unreferenced (Domain);
   begin
      return 1;
   end Get_First_CPU;

   ------------------
   -- Get_Last_CPU --
   ------------------

   function Get_Last_CPU (Domain : Dispatching_Domain) return CPU_Range is
      pragma Unreferenced (Domain);
   begin
      return 1;
   end Get_Last_CPU;

   -----------------------------
   -- Get_Dispatching_Domain --
   -----------------------------

   function Get_Dispatching_Domain
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task)
     return Dispatching_Domain
   is
      pragma Unreferenced (T);
   begin
      return System_Dispatching_Domain;
   end Get_Dispatching_Domain;

   -----------------
   -- Assign_Task --
   -----------------

   procedure Assign_Task
     (Domain : in Out Dispatching_Domain;
      CPU    : CPU_Range := Not_A_Specific_CPU;
      T      : Ada.Task_Identification.Task_Id :=
                 Ada.Task_Identification.Current_Task)
   is
      pragma Unreferenced (Domain, T);
   begin
      if CPU /= Not_A_Specific_CPU then
         Current_CPU := CPU;
      end if;
   end Assign_Task;

   -------------
   -- Set_CPU --
   -------------

   procedure Set_CPU
     (CPU : CPU_Range;
      T   : Ada.Task_Identification.Task_Id :=
              Ada.Task_Identification.Current_Task)
   is
      pragma Unreferenced (T);
   begin
      Current_CPU := CPU;
   end Set_CPU;

   -------------
   -- Get_CPU --
   -------------

   function Get_CPU
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task)
     return CPU_Range
   is
      pragma Unreferenced (T);
   begin
      return Current_CPU;
   end Get_CPU;

   ----------------------------
   -- Delay_Until_And_Set_CPU --
   ----------------------------

   procedure Delay_Until_And_Set_CPU
     (Delay_Until_Time : Ada.Real_Time.Time;
      CPU              : CPU_Range)
   is
   begin
      delay until Delay_Until_Time;
      Current_CPU := CPU;
   end Delay_Until_And_Set_CPU;

   ----------------------
   -- Interrupt_To_CPU --
   ----------------------

   function Interrupt_To_CPU (Interrupt : Positive) return CPU_Range is
      pragma Unreferenced (Interrupt);
   begin
      -- Single CPU, all interrupts go to CPU 1
      return 1;
   end Interrupt_To_CPU;

end Ada.Dispatching_Domains;
