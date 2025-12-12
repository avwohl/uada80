-- Ada.Dispatching.Non_Preemptive for Z80
-- Non-preemptive dispatching (Ada 2005)

package Ada.Dispatching.Non_Preemptive is
   pragma Preelaborate;

   -- Yield to other tasks at same priority
   procedure Yield_To_Higher;

   -- Yield to all ready tasks
   procedure Yield_To_Same_Or_Higher;

end Ada.Dispatching.Non_Preemptive;
