-- Ada.Dispatching_Domains for Z80
-- Multiprocessor dispatching domains

with System;
with Ada.Task_Identification;
with Ada.Real_Time;

package Ada.Dispatching_Domains is
   pragma Preelaborate;

   Dispatching_Domain_Error : exception;

   type Dispatching_Domain is limited private;

   System_Dispatching_Domain : constant Dispatching_Domain;

   type CPU_Range is range 0 .. 1;
   -- Z80 is single CPU
   Not_A_Specific_CPU : constant CPU_Range := 0;

   subtype CPU is CPU_Range range 1 .. 1;

   function Create (First, Last : CPU) return Dispatching_Domain;

   function Get_First_CPU (Domain : Dispatching_Domain) return CPU;
   function Get_Last_CPU (Domain : Dispatching_Domain) return CPU_Range;

   function Get_Dispatching_Domain
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task)
     return Dispatching_Domain;

   procedure Assign_Task
     (Domain : in Out Dispatching_Domain;
      CPU    : CPU_Range := Not_A_Specific_CPU;
      T      : Ada.Task_Identification.Task_Id :=
                 Ada.Task_Identification.Current_Task);

   procedure Set_CPU
     (CPU : CPU_Range;
      T   : Ada.Task_Identification.Task_Id :=
              Ada.Task_Identification.Current_Task);

   function Get_CPU
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task)
     return CPU_Range;

   procedure Delay_Until_And_Set_CPU
     (Delay_Until_Time : Ada.Real_Time.Time;
      CPU              : CPU_Range);

   function Interrupt_To_CPU (Interrupt : Positive) return CPU_Range;

private

   type Dispatching_Domain is new Integer range 0 .. 1;

   System_Dispatching_Domain : constant Dispatching_Domain := 0;

end Ada.Dispatching_Domains;
