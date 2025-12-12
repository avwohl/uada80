-- System.BB.Time for Z80
-- Bare board time management

package System.BB.Time is
   pragma Preelaborate;

   -- Time is measured in clock ticks
   type Time is new Natural;

   Time_First : constant Time := Time'First;
   Time_Last  : constant Time := Time'Last;

   -- Tick period in nanoseconds
   Tick_Period : constant := 10_000_000;  -- 10ms = 100 Hz

   -- Get current time
   function Clock return Time;

   -- Time arithmetic
   function "+" (Left : Time; Right : Time) return Time;
   function "-" (Left : Time; Right : Time) return Time;

   -- Delay until specified time
   procedure Delay_Until (T : Time);

   -- Initialize time subsystem
   procedure Initialize;

end System.BB.Time;
