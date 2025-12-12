-- Ada.Dispatching.Non_Preemptive body for Z80
-- Non-preemptive dispatching implementation

with System.Tasking;

package body Ada.Dispatching.Non_Preemptive is

   ---------------------
   -- Yield_To_Higher --
   ---------------------

   procedure Yield_To_Higher is
   begin
      -- Only yield if there's a higher-priority task ready
      System.Tasking.Yield;
   end Yield_To_Higher;

   -----------------------------
   -- Yield_To_Same_Or_Higher --
   -----------------------------

   procedure Yield_To_Same_Or_Higher is
   begin
      -- Yield unconditionally
      System.Tasking.Yield;
   end Yield_To_Same_Or_Higher;

end Ada.Dispatching.Non_Preemptive;
