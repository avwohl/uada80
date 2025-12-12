-- GNAT.Watchdog body for Z80
-- Watchdog timer implementation

package body GNAT.Watchdog is

   -- System tick counter (updated by interrupt)
   System_Tick : Natural := 0;
   pragma Volatile (System_Tick);

   -- Assumed tick rate (50 Hz for PAL systems)
   Tick_Rate : constant Natural := 50;

   ------------------
   -- Current_Tick --
   ------------------

   function Current_Tick return Natural is
   begin
      return System_Tick;
   end Current_Tick;

   ----------------------
   -- Ticks_Per_Second --
   ----------------------

   function Ticks_Per_Second return Natural is
   begin
      return Tick_Rate;
   end Ticks_Per_Second;

   ------------------
   -- MS_To_Ticks --
   ------------------

   function MS_To_Ticks (Milliseconds : Natural) return Natural is
   begin
      return (Milliseconds * Tick_Rate) / 1000;
   end MS_To_Ticks;

   -----------------
   -- Ticks_To_MS --
   -----------------

   function Ticks_To_MS (Ticks : Natural) return Natural is
   begin
      if Tick_Rate = 0 then
         return 0;
      end if;
      return (Ticks * 1000) / Tick_Rate;
   end Ticks_To_MS;

   ----------------------
   -- Seconds_To_Ticks --
   ----------------------

   function Seconds_To_Ticks (Seconds : Natural) return Natural is
   begin
      return Seconds * Tick_Rate;
   end Seconds_To_Ticks;

   --------------------
   -- Watchdog Timer --
   --------------------

   procedure Init_Watchdog (W : out Watchdog_Timer; Timeout_Ticks : Positive) is
   begin
      W.Timeout := Timeout_Ticks;
      W.Current := Timeout_Ticks;
      W.Running := False;
      W.Expired := False;
   end Init_Watchdog;

   procedure Reset_Watchdog (W : in Out Watchdog_Timer) is
   begin
      W.Current := W.Timeout;
      W.Expired := False;
   end Reset_Watchdog;

   procedure Start_Watchdog (W : in Out Watchdog_Timer) is
   begin
      W.Running := True;
      W.Expired := False;
   end Start_Watchdog;

   procedure Stop_Watchdog (W : in Out Watchdog_Timer) is
   begin
      W.Running := False;
   end Stop_Watchdog;

   function Is_Expired (W : Watchdog_Timer) return Boolean is
   begin
      return W.Expired;
   end Is_Expired;

   function Is_Running (W : Watchdog_Timer) return Boolean is
   begin
      return W.Running;
   end Is_Running;

   function Remaining_Ticks (W : Watchdog_Timer) return Natural is
   begin
      return W.Current;
   end Remaining_Ticks;

   function Elapsed_Ticks (W : Watchdog_Timer) return Natural is
   begin
      return W.Timeout - W.Current;
   end Elapsed_Ticks;

   procedure Update_Watchdog (W : in Out Watchdog_Timer) is
   begin
      if W.Running and not W.Expired then
         if W.Current > 0 then
            W.Current := W.Current - 1;
         else
            W.Expired := True;
         end if;
      end if;
   end Update_Watchdog;

   ---------------------
   -- Countdown Timer --
   ---------------------

   procedure Init_Countdown (C : out Countdown_Timer; Ticks : Positive) is
   begin
      C.Initial := Ticks;
      C.Remaining := Ticks;
      C.Running := False;
      C.Paused := False;
   end Init_Countdown;

   procedure Start_Countdown (C : in Out Countdown_Timer) is
   begin
      C.Running := True;
      C.Paused := False;
   end Start_Countdown;

   procedure Pause_Countdown (C : in Out Countdown_Timer) is
   begin
      if C.Running then
         C.Paused := True;
      end if;
   end Pause_Countdown;

   procedure Resume_Countdown (C : in Out Countdown_Timer) is
   begin
      if C.Running then
         C.Paused := False;
      end if;
   end Resume_Countdown;

   procedure Reset_Countdown (C : in Out Countdown_Timer) is
   begin
      C.Remaining := C.Initial;
      C.Running := False;
      C.Paused := False;
   end Reset_Countdown;

   function Countdown_Expired (C : Countdown_Timer) return Boolean is
   begin
      return C.Running and C.Remaining = 0;
   end Countdown_Expired;

   function Countdown_Remaining (C : Countdown_Timer) return Natural is
   begin
      return C.Remaining;
   end Countdown_Remaining;

   function Countdown_Is_Paused (C : Countdown_Timer) return Boolean is
   begin
      return C.Paused;
   end Countdown_Is_Paused;

   procedure Update_Countdown (C : in Out Countdown_Timer) is
   begin
      if C.Running and not C.Paused and C.Remaining > 0 then
         C.Remaining := C.Remaining - 1;
      end if;
   end Update_Countdown;

   ------------------
   -- Timer System --
   ------------------

   procedure Init_Timer_System (T : out Timer_System) is
   begin
      T.Count := 0;
      for I in 1 .. Max_Timers loop
         T.Timers (I).Used := False;
         T.Timers (I).Active := False;
         T.Timers (I).Expired := False;
         T.Timers (I).Timeout := 0;
         T.Timers (I).Current := 0;
      end loop;
   end Init_Timer_System;

   function Create_Timer (T : in Out Timer_System;
                          Timeout_Ticks : Positive) return Natural is
   begin
      for I in 1 .. Max_Timers loop
         if not T.Timers (I).Used then
            T.Timers (I).Used := True;
            T.Timers (I).Timeout := Timeout_Ticks;
            T.Timers (I).Current := Timeout_Ticks;
            T.Timers (I).Active := False;
            T.Timers (I).Expired := False;
            T.Count := T.Count + 1;
            return I;
         end if;
      end loop;
      return 0;  -- No free timers
   end Create_Timer;

   procedure Destroy_Timer (T : in Out Timer_System; Timer_Id : Natural) is
   begin
      if Timer_Id in 1 .. Max_Timers and then T.Timers (Timer_Id).Used then
         T.Timers (Timer_Id).Used := False;
         T.Timers (Timer_Id).Active := False;
         T.Count := T.Count - 1;
      end if;
   end Destroy_Timer;

   procedure Reset_Timer (T : in Out Timer_System; Timer_Id : Natural) is
   begin
      if Timer_Id in 1 .. Max_Timers and then T.Timers (Timer_Id).Used then
         T.Timers (Timer_Id).Current := T.Timers (Timer_Id).Timeout;
         T.Timers (Timer_Id).Expired := False;
      end if;
   end Reset_Timer;

   procedure Start_Timer (T : in Out Timer_System; Timer_Id : Natural) is
   begin
      if Timer_Id in 1 .. Max_Timers and then T.Timers (Timer_Id).Used then
         T.Timers (Timer_Id).Active := True;
         T.Timers (Timer_Id).Expired := False;
      end if;
   end Start_Timer;

   procedure Stop_Timer (T : in Out Timer_System; Timer_Id : Natural) is
   begin
      if Timer_Id in 1 .. Max_Timers and then T.Timers (Timer_Id).Used then
         T.Timers (Timer_Id).Active := False;
      end if;
   end Stop_Timer;

   function Timer_Expired (T : Timer_System; Timer_Id : Natural) return Boolean is
   begin
      if Timer_Id not in 1 .. Max_Timers then
         return False;
      end if;
      return T.Timers (Timer_Id).Used and T.Timers (Timer_Id).Expired;
   end Timer_Expired;

   function Timer_Remaining (T : Timer_System; Timer_Id : Natural) return Natural is
   begin
      if Timer_Id not in 1 .. Max_Timers or else not T.Timers (Timer_Id).Used then
         return 0;
      end if;
      return T.Timers (Timer_Id).Current;
   end Timer_Remaining;

   function Timer_Is_Active (T : Timer_System; Timer_Id : Natural) return Boolean is
   begin
      if Timer_Id not in 1 .. Max_Timers then
         return False;
      end if;
      return T.Timers (Timer_Id).Used and T.Timers (Timer_Id).Active;
   end Timer_Is_Active;

   procedure Update_All_Timers (T : in Out Timer_System) is
   begin
      for I in 1 .. Max_Timers loop
         if T.Timers (I).Used and T.Timers (I).Active and
            not T.Timers (I).Expired then
            if T.Timers (I).Current > 0 then
               T.Timers (I).Current := T.Timers (I).Current - 1;
            else
               T.Timers (I).Expired := True;
            end if;
         end if;
      end loop;
   end Update_All_Timers;

   function Active_Timer_Count (T : Timer_System) return Natural is
      Count : Natural := 0;
   begin
      for I in 1 .. Max_Timers loop
         if T.Timers (I).Used and T.Timers (I).Active then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Active_Timer_Count;

   function Any_Timer_Expired (T : Timer_System) return Boolean is
   begin
      for I in 1 .. Max_Timers loop
         if T.Timers (I).Used and T.Timers (I).Expired then
            return True;
         end if;
      end loop;
      return False;
   end Any_Timer_Expired;

   function First_Expired_Timer (T : Timer_System) return Natural is
   begin
      for I in 1 .. Max_Timers loop
         if T.Timers (I).Used and T.Timers (I).Expired then
            return I;
         end if;
      end loop;
      return 0;
   end First_Expired_Timer;

   --------------------
   -- Interval Timer --
   --------------------

   procedure Init_Interval (I : out Interval_Timer; Interval_Ticks : Positive) is
   begin
      I.Interval := Interval_Ticks;
      I.Current := Interval_Ticks;
      I.Running := False;
      I.Triggered := False;
      I.Trigger_Count := 0;
   end Init_Interval;

   procedure Start_Interval (I : in Out Interval_Timer) is
   begin
      I.Running := True;
   end Start_Interval;

   procedure Stop_Interval (I : in Out Interval_Timer) is
   begin
      I.Running := False;
   end Stop_Interval;

   function Interval_Triggered (I : Interval_Timer) return Boolean is
   begin
      return I.Triggered;
   end Interval_Triggered;

   procedure Clear_Interval_Trigger (I : in Out Interval_Timer) is
   begin
      I.Triggered := False;
   end Clear_Interval_Trigger;

   procedure Update_Interval (I : in Out Interval_Timer) is
   begin
      if I.Running then
         if I.Current > 0 then
            I.Current := I.Current - 1;
         else
            I.Triggered := True;
            I.Trigger_Count := I.Trigger_Count + 1;
            I.Current := I.Interval;  -- Reset for next interval
         end if;
      end if;
   end Update_Interval;

   function Interval_Count (I : Interval_Timer) return Natural is
   begin
      return I.Trigger_Count;
   end Interval_Count;

   procedure Reset_Interval_Count (I : in Out Interval_Timer) is
   begin
      I.Trigger_Count := 0;
   end Reset_Interval_Count;

   ----------------------
   -- Deadline Tracker --
   ----------------------

   procedure Set_Deadline (D : out Deadline; Ticks_From_Now : Natural) is
   begin
      D.Target_Tick := System_Tick + Ticks_From_Now;
      D.Set := True;
   end Set_Deadline;

   function Deadline_Passed (D : Deadline) return Boolean is
   begin
      if not D.Set then
         return False;
      end if;
      return System_Tick >= D.Target_Tick;
   end Deadline_Passed;

   function Ticks_Until_Deadline (D : Deadline) return Natural is
   begin
      if not D.Set or System_Tick >= D.Target_Tick then
         return 0;
      end if;
      return D.Target_Tick - System_Tick;
   end Ticks_Until_Deadline;

   function Ticks_Past_Deadline (D : Deadline) return Natural is
   begin
      if not D.Set or System_Tick < D.Target_Tick then
         return 0;
      end if;
      return System_Tick - D.Target_Tick;
   end Ticks_Past_Deadline;

end GNAT.Watchdog;
