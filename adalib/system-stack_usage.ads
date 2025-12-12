-- System.Stack_Usage for Z80
-- Stack usage tracking

package System.Stack_Usage is
   pragma Preelaborate;

   -- Stack usage statistics
   type Stack_Stats is record
      Stack_Base   : System.Address;
      Stack_Size   : Natural;
      Peak_Usage   : Natural;
      Current_Used : Natural;
   end record;

   -- Initialize stack tracking for task
   procedure Initialize
     (Stats : out Stack_Stats;
      Base  : System.Address;
      Size  : Natural);

   -- Update stack usage statistics
   procedure Update (Stats : in Out Stack_Stats; Current_SP : System.Address);

   -- Report stack usage
   procedure Report (Stats : Stack_Stats);

   -- Fill stack with pattern for tracking
   procedure Fill_Stack (Base : System.Address; Size : Natural);

   -- Calculate actual stack usage from fill pattern
   function Compute_Usage (Stats : Stack_Stats) return Natural;

end System.Stack_Usage;
