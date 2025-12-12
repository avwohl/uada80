-- Ada.Calendar for Z80
-- Provides time and date operations
--
-- Note: Z80 systems typically don't have real-time clocks.
-- This implementation provides a basic tick-based timer.

package Ada.Calendar is
   pragma Preelaborate;

   -- Time type represents elapsed time since system start
   -- Stored as ticks (system dependent resolution)
   type Time is private;

   -- Duration in seconds (simplified fixed-point for Z80)
   -- Using 16-bit integer for simplicity
   subtype Year_Number  is Integer range 1901 .. 2099;
   subtype Month_Number is Integer range 1 .. 12;
   subtype Day_Number   is Integer range 1 .. 31;
   subtype Day_Duration is Integer range 0 .. 86400;

   -- Days in each month (non-leap year)
   type Month_Days is array (Month_Number) of Day_Number;

   Days_In_Month : constant Month_Days :=
     (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

   -- Clock function returns current time
   function Clock return Time;

   -- Time component extraction
   function Year    (Date : Time) return Year_Number;
   function Month   (Date : Time) return Month_Number;
   function Day     (Date : Time) return Day_Number;
   function Seconds (Date : Time) return Day_Duration;

   -- Combined split
   procedure Split
     (Date    : Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration);

   -- Time construction
   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0) return Time;

   -- Arithmetic operations
   function "+" (Left : Time; Right : Integer) return Time;
   function "+" (Left : Integer; Right : Time) return Time;
   function "-" (Left : Time; Right : Integer) return Time;
   function "-" (Left : Time; Right : Time) return Integer;

   -- Comparison
   function "<"  (Left, Right : Time) return Boolean;
   function "<=" (Left, Right : Time) return Boolean;
   function ">"  (Left, Right : Time) return Boolean;
   function ">=" (Left, Right : Time) return Boolean;

   -- Exception for invalid time values
   Time_Error : exception;

private

   -- Time stored as ticks since epoch (16-bit for Z80)
   -- Each tick = 1/50th second (20ms) on typical Z80 systems
   type Time is record
      Ticks : Integer := 0;  -- Tick count
      Days  : Integer := 0;  -- Days since epoch (allows > 655 seconds)
   end record;

end Ada.Calendar;
