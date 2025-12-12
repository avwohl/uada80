-- Ada.Calendar.Arithmetic for Z80
-- Calendar arithmetic operations

with Ada.Calendar;

package Ada.Calendar.Arithmetic is
   pragma Preelaborate;

   -- Day count type for differences
   type Day_Count is range -366 * (2399 - 1901 + 1) .. 366 * (2399 - 1901 + 1);

   -- Leap seconds count
   subtype Leap_Seconds_Count is Integer range -2047 .. 2047;

   -- Add days to time
   function "+" (Left : Time; Right : Day_Count) return Time;
   function "+" (Left : Day_Count; Right : Time) return Time;
   function "-" (Left : Time; Right : Day_Count) return Time;

   -- Difference in days
   function "-" (Left, Right : Time) return Day_Count;

   -- Full difference function
   procedure Difference
     (Left         : Time;
      Right        : Time;
      Days         : out Day_Count;
      Seconds      : out Duration;
      Leap_Seconds : out Leap_Seconds_Count);

end Ada.Calendar.Arithmetic;
