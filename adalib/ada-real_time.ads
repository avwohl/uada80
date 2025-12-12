-- Ada.Real_Time for Z80
-- High-resolution time and timing events
--
-- Provides monotonic time for real-time programming

package Ada.Real_Time is
   pragma Preelaborate;

   -- Time represents a point in time (monotonic clock)
   type Time is private;

   Time_First : constant Time;
   Time_Last  : constant Time;
   Time_Unit  : constant := 0.02;  -- 20ms per tick (50Hz)

   -- Time_Span represents a duration
   type Time_Span is private;

   Time_Span_First : constant Time_Span;
   Time_Span_Last  : constant Time_Span;
   Time_Span_Zero  : constant Time_Span;
   Time_Span_Unit  : constant Time_Span;

   Tick : constant Time_Span;

   -- Clock function
   function Clock return Time;

   -- Time_Span construction
   function Nanoseconds  (NS : Integer) return Time_Span;
   function Microseconds (US : Integer) return Time_Span;
   function Milliseconds (MS : Integer) return Time_Span;
   function Seconds      (S  : Integer) return Time_Span;
   function Minutes      (M  : Integer) return Time_Span;

   -- Time_Span conversion
   function To_Duration (TS : Time_Span) return Duration;
   function To_Time_Span (D : Duration) return Time_Span;

   -- Time operations
   function "+" (Left : Time; Right : Time_Span) return Time;
   function "+" (Left : Time_Span; Right : Time) return Time;
   function "-" (Left : Time; Right : Time_Span) return Time;
   function "-" (Left : Time; Right : Time) return Time_Span;

   -- Time comparison
   function "<"  (Left, Right : Time) return Boolean;
   function "<=" (Left, Right : Time) return Boolean;
   function ">"  (Left, Right : Time) return Boolean;
   function ">=" (Left, Right : Time) return Boolean;

   -- Time_Span operations
   function "+" (Left, Right : Time_Span) return Time_Span;
   function "-" (Left, Right : Time_Span) return Time_Span;
   function "-" (Right : Time_Span) return Time_Span;
   function "*" (Left : Time_Span; Right : Integer) return Time_Span;
   function "*" (Left : Integer; Right : Time_Span) return Time_Span;
   function "/" (Left, Right : Time_Span) return Integer;
   function "/" (Left : Time_Span; Right : Integer) return Time_Span;

   function "abs" (Right : Time_Span) return Time_Span;

   -- Time_Span comparison
   function "<"  (Left, Right : Time_Span) return Boolean;
   function "<=" (Left, Right : Time_Span) return Boolean;
   function ">"  (Left, Right : Time_Span) return Boolean;
   function ">=" (Left, Right : Time_Span) return Boolean;

   -- Split time into seconds and fraction
   procedure Split (T : Time; SC : out Integer; TS : out Time_Span);
   function Time_Of (SC : Integer; TS : Time_Span) return Time;

   -- Seconds count type
   type Seconds_Count is range -32768 .. 32767;

private

   -- Time represented as ticks since system start
   -- Each tick = 20ms (50Hz), giving ~21 minutes in 16-bit
   type Time is record
      Ticks : Integer := 0;
   end record;

   -- Time_Span as tick count
   type Time_Span is record
      Ticks : Integer := 0;
   end record;

   Time_First : constant Time := (Ticks => Integer'First);
   Time_Last  : constant Time := (Ticks => Integer'Last);

   Time_Span_First : constant Time_Span := (Ticks => Integer'First);
   Time_Span_Last  : constant Time_Span := (Ticks => Integer'Last);
   Time_Span_Zero  : constant Time_Span := (Ticks => 0);
   Time_Span_Unit  : constant Time_Span := (Ticks => 1);

   Tick : constant Time_Span := (Ticks => 1);

end Ada.Real_Time;
