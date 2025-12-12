-- GNAT.Interval_Timer for Z80
-- Simple interval timing for CP/M using system ticks

package GNAT.Interval_Timer is
   pragma Preelaborate;

   type Timer is private;
   type Duration_Ticks is new Natural;

   -- CP/M typically has 50Hz or 60Hz tick rate
   Ticks_Per_Second : constant := 50;

   procedure Start (T : out Timer);
   --  Start timing

   procedure Stop (T : in Out Timer);
   --  Stop timing

   procedure Reset (T : out Timer);
   --  Reset timer

   function Elapsed (T : Timer) return Duration_Ticks;
   --  Return elapsed ticks since start

   function Elapsed_Seconds (T : Timer) return Natural;
   --  Return elapsed whole seconds

   function Elapsed_Tenths (T : Timer) return Natural;
   --  Return elapsed time in tenths of seconds

   function Is_Running (T : Timer) return Boolean;
   --  Check if timer is running

   -- Countdown timer support
   procedure Set_Countdown (T : out Timer; Ticks : Duration_Ticks);
   --  Set countdown timer

   function Expired (T : Timer) return Boolean;
   --  Check if countdown has expired

   function Remaining (T : Timer) return Duration_Ticks;
   --  Return remaining ticks in countdown

   -- Multiple timer management
   Max_Timers : constant := 8;

   type Timer_ID is range 0 .. Max_Timers;
   Invalid_Timer : constant Timer_ID := 0;

   type Timer_Callback is access procedure (ID : Timer_ID);

   function Allocate_Timer return Timer_ID;
   --  Allocate a timer slot

   procedure Free_Timer (ID : Timer_ID);
   --  Free a timer slot

   procedure Start_Timer (ID : Timer_ID; Period : Duration_Ticks;
                          Callback : Timer_Callback := null;
                          Periodic : Boolean := False);
   --  Start timer with callback

   procedure Stop_Timer (ID : Timer_ID);
   --  Stop timer

   procedure Check_Timers;
   --  Check all timers and invoke callbacks (call from main loop)

   -- Current time support
   function Current_Ticks return Duration_Ticks;
   --  Return current system tick count

   procedure Delay_Ticks (Ticks : Duration_Ticks);
   --  Delay for specified ticks (busy wait)

   procedure Delay_Seconds (Seconds : Positive);
   --  Delay for specified seconds

private

   type Timer is record
      Start_Tick  : Duration_Ticks := 0;
      Stop_Tick   : Duration_Ticks := 0;
      Countdown   : Duration_Ticks := 0;
      Running     : Boolean := False;
      Is_Countdown : Boolean := False;
   end record;

   type Timer_Slot is record
      Target    : Duration_Ticks := 0;
      Period    : Duration_Ticks := 0;
      Callback  : Timer_Callback := null;
      Periodic  : Boolean := False;
      Active    : Boolean := False;
      Allocated : Boolean := False;
   end record;

   type Timer_Array is array (1 .. Max_Timers) of Timer_Slot;

end GNAT.Interval_Timer;
