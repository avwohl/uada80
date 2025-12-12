-- GNAT.Timer for Z80
-- Simple elapsed time measurement using tick counter

package GNAT.Timer is
   pragma Preelaborate;

   -- Timer record for measuring elapsed time
   -- Uses system tick counter (typically incremented by interrupt)
   type Timer_Type is limited private;

   -- Tick frequency (user should set this based on system configuration)
   -- Default assumes 50 Hz (50 ticks per second, common for Z80 systems)
   Ticks_Per_Second : constant := 50;

   -- Initialize/reset timer
   procedure Start (T : out Timer_Type);

   -- Stop timer (freeze elapsed time)
   procedure Stop (T : in out Timer_Type);

   -- Resume a stopped timer
   procedure Resume (T : in Out Timer_Type);

   -- Reset timer to zero
   procedure Reset (T : out Timer_Type);

   -- Get elapsed time in ticks
   function Elapsed_Ticks (T : Timer_Type) return Natural;

   -- Get elapsed time in milliseconds (approximate)
   function Elapsed_MS (T : Timer_Type) return Natural;

   -- Get elapsed time in seconds (truncated)
   function Elapsed_Seconds (T : Timer_Type) return Natural;

   -- Check if specific duration has passed
   function Has_Elapsed_MS (T : Timer_Type; MS : Natural) return Boolean;
   function Has_Elapsed_Ticks (T : Timer_Type; Ticks : Natural) return Boolean;
   function Has_Elapsed_Seconds (T : Timer_Type; Seconds : Natural) return Boolean;

   -- Is timer running?
   function Is_Running (T : Timer_Type) return Boolean;

   -- Get current system tick count (reads from system)
   function Current_Ticks return Natural;

   -- Delay operations (busy wait)
   procedure Delay_Ticks (Ticks : Natural);
   procedure Delay_MS (MS : Natural);

   -- Countdown timer type
   type Countdown is limited private;

   -- Initialize countdown with duration
   procedure Set_Countdown (C : out Countdown; MS : Natural);
   procedure Set_Countdown_Ticks (C : out Countdown; Ticks : Natural);

   -- Check if countdown has expired
   function Has_Expired (C : Countdown) return Boolean;

   -- Get remaining time
   function Remaining_MS (C : Countdown) return Natural;
   function Remaining_Ticks (C : Countdown) return Natural;

   -- Reset countdown to original duration
   procedure Reset_Countdown (C : in Out Countdown);

private

   type Timer_Type is limited record
      Start_Tick   : Natural := 0;
      Accumulated  : Natural := 0;
      Running      : Boolean := False;
   end record;

   type Countdown is limited record
      Start_Tick : Natural := 0;
      Duration   : Natural := 0;
   end record;

end GNAT.Timer;
