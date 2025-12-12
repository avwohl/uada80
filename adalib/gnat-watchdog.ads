-- GNAT.Watchdog for Z80
-- Watchdog timer and timeout management

package GNAT.Watchdog is
   pragma Preelaborate;

   -- Simple watchdog timer
   type Watchdog_Timer is limited private;

   procedure Init_Watchdog (W : out Watchdog_Timer; Timeout_Ticks : Positive);
   procedure Reset_Watchdog (W : in Out Watchdog_Timer);
   procedure Start_Watchdog (W : in Out Watchdog_Timer);
   procedure Stop_Watchdog (W : in Out Watchdog_Timer);
   function Is_Expired (W : Watchdog_Timer) return Boolean;
   function Is_Running (W : Watchdog_Timer) return Boolean;
   function Remaining_Ticks (W : Watchdog_Timer) return Natural;
   function Elapsed_Ticks (W : Watchdog_Timer) return Natural;
   procedure Update_Watchdog (W : in Out Watchdog_Timer);  -- Call periodically

   -- Countdown timer
   type Countdown_Timer is limited private;

   procedure Init_Countdown (C : out Countdown_Timer; Ticks : Positive);
   procedure Start_Countdown (C : in Out Countdown_Timer);
   procedure Pause_Countdown (C : in Out Countdown_Timer);
   procedure Resume_Countdown (C : in Out Countdown_Timer);
   procedure Reset_Countdown (C : in Out Countdown_Timer);
   function Countdown_Expired (C : Countdown_Timer) return Boolean;
   function Countdown_Remaining (C : Countdown_Timer) return Natural;
   function Countdown_Is_Paused (C : Countdown_Timer) return Boolean;
   procedure Update_Countdown (C : in Out Countdown_Timer);

   -- Multi-timer system (8 independent timers)
   Max_Timers : constant := 8;

   type Timer_System is limited private;

   procedure Init_Timer_System (T : out Timer_System);
   function Create_Timer (T : in Out Timer_System;
                          Timeout_Ticks : Positive) return Natural;
   procedure Destroy_Timer (T : in Out Timer_System; Timer_Id : Natural);
   procedure Reset_Timer (T : in Out Timer_System; Timer_Id : Natural);
   procedure Start_Timer (T : in Out Timer_System; Timer_Id : Natural);
   procedure Stop_Timer (T : in Out Timer_System; Timer_Id : Natural);
   function Timer_Expired (T : Timer_System; Timer_Id : Natural) return Boolean;
   function Timer_Remaining (T : Timer_System; Timer_Id : Natural) return Natural;
   function Timer_Is_Active (T : Timer_System; Timer_Id : Natural) return Boolean;
   procedure Update_All_Timers (T : in Out Timer_System);
   function Active_Timer_Count (T : Timer_System) return Natural;
   function Any_Timer_Expired (T : Timer_System) return Boolean;
   function First_Expired_Timer (T : Timer_System) return Natural;

   -- Interval timer (repeating)
   type Interval_Timer is limited private;

   procedure Init_Interval (I : out Interval_Timer; Interval_Ticks : Positive);
   procedure Start_Interval (I : in Out Interval_Timer);
   procedure Stop_Interval (I : in Out Interval_Timer);
   function Interval_Triggered (I : Interval_Timer) return Boolean;
   procedure Clear_Interval_Trigger (I : in Out Interval_Timer);
   procedure Update_Interval (I : in Out Interval_Timer);
   function Interval_Count (I : Interval_Timer) return Natural;
   procedure Reset_Interval_Count (I : in Out Interval_Timer);

   -- Deadline tracker
   type Deadline is limited private;

   procedure Set_Deadline (D : out Deadline; Ticks_From_Now : Natural);
   function Deadline_Passed (D : Deadline) return Boolean;
   function Ticks_Until_Deadline (D : Deadline) return Natural;
   function Ticks_Past_Deadline (D : Deadline) return Natural;

   -- Time utilities
   function Current_Tick return Natural;
   function Ticks_Per_Second return Natural;
   function MS_To_Ticks (Milliseconds : Natural) return Natural;
   function Ticks_To_MS (Ticks : Natural) return Natural;
   function Seconds_To_Ticks (Seconds : Natural) return Natural;

private

   type Watchdog_Timer is limited record
      Timeout   : Natural;
      Current   : Natural;
      Running   : Boolean;
      Expired   : Boolean;
   end record;

   type Countdown_Timer is limited record
      Initial   : Natural;
      Remaining : Natural;
      Running   : Boolean;
      Paused    : Boolean;
   end record;

   type Timer_Record is record
      Timeout   : Natural;
      Current   : Natural;
      Active    : Boolean;
      Expired   : Boolean;
      Used      : Boolean;
   end record;
   type Timer_Array is array (1 .. Max_Timers) of Timer_Record;

   type Timer_System is limited record
      Timers : Timer_Array;
      Count  : Natural := 0;
   end record;

   type Interval_Timer is limited record
      Interval    : Natural;
      Current     : Natural;
      Running     : Boolean;
      Triggered   : Boolean;
      Trigger_Count : Natural;
   end record;

   type Deadline is limited record
      Target_Tick : Natural;
      Set         : Boolean;
   end record;

end GNAT.Watchdog;
