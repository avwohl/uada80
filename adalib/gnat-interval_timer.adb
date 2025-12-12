-- GNAT.Interval_Timer body for Z80
-- Simple interval timing implementation for CP/M

package body GNAT.Interval_Timer is

   Timers : Timer_Array;
   System_Ticks : Duration_Ticks := 0;  -- Simulated, should use real ticks

   -- Read CP/M system tick counter
   -- On real CP/M this would read from BDOS or a system location
   function Read_System_Ticks return Duration_Ticks is
   begin
      -- In a real implementation, this would read from the CP/M tick counter
      -- at location 0x0000 or use BDOS function
      -- For now, increment on each call
      System_Ticks := System_Ticks + 1;
      return System_Ticks;
   end Read_System_Ticks;

   -----------
   -- Start --
   -----------

   procedure Start (T : out Timer) is
   begin
      T.Start_Tick := Read_System_Ticks;
      T.Running := True;
      T.Is_Countdown := False;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (T : in Out Timer) is
   begin
      if T.Running then
         T.Stop_Tick := Read_System_Ticks;
         T.Running := False;
      end if;
   end Stop;

   -----------
   -- Reset --
   -----------

   procedure Reset (T : out Timer) is
   begin
      T := (Start_Tick   => 0,
            Stop_Tick    => 0,
            Countdown    => 0,
            Running      => False,
            Is_Countdown => False);
   end Reset;

   -------------
   -- Elapsed --
   -------------

   function Elapsed (T : Timer) return Duration_Ticks is
      Current : Duration_Ticks;
   begin
      if T.Running then
         Current := Read_System_Ticks;
         return Current - T.Start_Tick;
      else
         return T.Stop_Tick - T.Start_Tick;
      end if;
   end Elapsed;

   ---------------------
   -- Elapsed_Seconds --
   ---------------------

   function Elapsed_Seconds (T : Timer) return Natural is
   begin
      return Natural (Elapsed (T)) / Ticks_Per_Second;
   end Elapsed_Seconds;

   --------------------
   -- Elapsed_Tenths --
   --------------------

   function Elapsed_Tenths (T : Timer) return Natural is
   begin
      return (Natural (Elapsed (T)) * 10) / Ticks_Per_Second;
   end Elapsed_Tenths;

   ----------------
   -- Is_Running --
   ----------------

   function Is_Running (T : Timer) return Boolean is
   begin
      return T.Running;
   end Is_Running;

   -------------------
   -- Set_Countdown --
   -------------------

   procedure Set_Countdown (T : out Timer; Ticks : Duration_Ticks) is
   begin
      T.Start_Tick := Read_System_Ticks;
      T.Countdown := Ticks;
      T.Running := True;
      T.Is_Countdown := True;
   end Set_Countdown;

   -------------
   -- Expired --
   -------------

   function Expired (T : Timer) return Boolean is
   begin
      if not T.Is_Countdown then
         return False;
      end if;
      return Elapsed (T) >= T.Countdown;
   end Expired;

   ---------------
   -- Remaining --
   ---------------

   function Remaining (T : Timer) return Duration_Ticks is
      Elap : Duration_Ticks;
   begin
      if not T.Is_Countdown then
         return 0;
      end if;
      Elap := Elapsed (T);
      if Elap >= T.Countdown then
         return 0;
      else
         return T.Countdown - Elap;
      end if;
   end Remaining;

   --------------------
   -- Allocate_Timer --
   --------------------

   function Allocate_Timer return Timer_ID is
   begin
      for I in 1 .. Max_Timers loop
         if not Timers (I).Allocated then
            Timers (I).Allocated := True;
            Timers (I).Active := False;
            return Timer_ID (I);
         end if;
      end loop;
      return Invalid_Timer;
   end Allocate_Timer;

   ----------------
   -- Free_Timer --
   ----------------

   procedure Free_Timer (ID : Timer_ID) is
   begin
      if ID in 1 .. Max_Timers then
         Timers (Positive (ID)).Allocated := False;
         Timers (Positive (ID)).Active := False;
      end if;
   end Free_Timer;

   -----------------
   -- Start_Timer --
   -----------------

   procedure Start_Timer (ID : Timer_ID; Period : Duration_Ticks;
                          Callback : Timer_Callback := null;
                          Periodic : Boolean := False)
   is
   begin
      if ID in 1 .. Max_Timers and then Timers (Positive (ID)).Allocated then
         Timers (Positive (ID)).Target := Read_System_Ticks + Period;
         Timers (Positive (ID)).Period := Period;
         Timers (Positive (ID)).Callback := Callback;
         Timers (Positive (ID)).Periodic := Periodic;
         Timers (Positive (ID)).Active := True;
      end if;
   end Start_Timer;

   ----------------
   -- Stop_Timer --
   ----------------

   procedure Stop_Timer (ID : Timer_ID) is
   begin
      if ID in 1 .. Max_Timers then
         Timers (Positive (ID)).Active := False;
      end if;
   end Stop_Timer;

   ------------------
   -- Check_Timers --
   ------------------

   procedure Check_Timers is
      Current : constant Duration_Ticks := Read_System_Ticks;
   begin
      for I in 1 .. Max_Timers loop
         if Timers (I).Active and then Current >= Timers (I).Target then
            if Timers (I).Callback /= null then
               Timers (I).Callback (Timer_ID (I));
            end if;

            if Timers (I).Periodic then
               Timers (I).Target := Current + Timers (I).Period;
            else
               Timers (I).Active := False;
            end if;
         end if;
      end loop;
   end Check_Timers;

   -------------------
   -- Current_Ticks --
   -------------------

   function Current_Ticks return Duration_Ticks is
   begin
      return Read_System_Ticks;
   end Current_Ticks;

   -----------------
   -- Delay_Ticks --
   -----------------

   procedure Delay_Ticks (Ticks : Duration_Ticks) is
      Target : constant Duration_Ticks := Read_System_Ticks + Ticks;
   begin
      while Read_System_Ticks < Target loop
         null;  -- Busy wait
      end loop;
   end Delay_Ticks;

   -------------------
   -- Delay_Seconds --
   -------------------

   procedure Delay_Seconds (Seconds : Positive) is
   begin
      Delay_Ticks (Duration_Ticks (Seconds * Ticks_Per_Second));
   end Delay_Seconds;

end GNAT.Interval_Timer;
