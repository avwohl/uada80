-- GNAT.Timer body for Z80
-- Timer implementation using system tick counter

with System.Machine_Code;

package body GNAT.Timer is

   -- Z80 system tick counter location (typical CP/M location)
   -- This should be configured based on the actual system
   Tick_Counter_Addr : constant := 16#0005#;  -- Placeholder

   -------------------
   -- Current_Ticks --
   -------------------

   function Current_Ticks return Natural is
      -- Read tick counter from system
      -- For CP/M, this might read from a BIOS location or use a custom routine
      -- This is a simplified implementation
      Ticks : Natural := 0;
   begin
      -- In a real implementation, this would read from hardware or system memory
      -- For now, return a dummy value that increments
      -- Users should replace this with actual tick source

      -- Placeholder: read from memory location
      declare
         type Byte is mod 256;
         type Word is mod 65536;
         Tick_Loc : Word;
         for Tick_Loc'Address use System'To_Address (Tick_Counter_Addr);
         pragma Volatile (Tick_Loc);
      begin
         Ticks := Natural (Tick_Loc);
      end;

      return Ticks;
   exception
      when others => return 0;
   end Current_Ticks;

   -----------
   -- Start --
   -----------

   procedure Start (T : out Timer_Type) is
   begin
      T.Start_Tick := Current_Ticks;
      T.Accumulated := 0;
      T.Running := True;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (T : in Out Timer_Type) is
   begin
      if T.Running then
         T.Accumulated := T.Accumulated + (Current_Ticks - T.Start_Tick);
         T.Running := False;
      end if;
   end Stop;

   ------------
   -- Resume --
   ------------

   procedure Resume (T : in Out Timer_Type) is
   begin
      if not T.Running then
         T.Start_Tick := Current_Ticks;
         T.Running := True;
      end if;
   end Resume;

   -----------
   -- Reset --
   -----------

   procedure Reset (T : out Timer_Type) is
   begin
      T.Start_Tick := Current_Ticks;
      T.Accumulated := 0;
      T.Running := False;
   end Reset;

   -------------------
   -- Elapsed_Ticks --
   -------------------

   function Elapsed_Ticks (T : Timer_Type) return Natural is
   begin
      if T.Running then
         return T.Accumulated + (Current_Ticks - T.Start_Tick);
      else
         return T.Accumulated;
      end if;
   end Elapsed_Ticks;

   ----------------
   -- Elapsed_MS --
   ----------------

   function Elapsed_MS (T : Timer_Type) return Natural is
      Ticks : constant Natural := Elapsed_Ticks (T);
   begin
      -- Convert ticks to milliseconds
      return (Ticks * 1000) / Ticks_Per_Second;
   end Elapsed_MS;

   ---------------------
   -- Elapsed_Seconds --
   ---------------------

   function Elapsed_Seconds (T : Timer_Type) return Natural is
   begin
      return Elapsed_Ticks (T) / Ticks_Per_Second;
   end Elapsed_Seconds;

   --------------------
   -- Has_Elapsed_MS --
   --------------------

   function Has_Elapsed_MS (T : Timer_Type; MS : Natural) return Boolean is
   begin
      return Elapsed_MS (T) >= MS;
   end Has_Elapsed_MS;

   -----------------------
   -- Has_Elapsed_Ticks --
   -----------------------

   function Has_Elapsed_Ticks (T : Timer_Type; Ticks : Natural) return Boolean is
   begin
      return Elapsed_Ticks (T) >= Ticks;
   end Has_Elapsed_Ticks;

   -------------------------
   -- Has_Elapsed_Seconds --
   -------------------------

   function Has_Elapsed_Seconds (T : Timer_Type; Seconds : Natural) return Boolean is
   begin
      return Elapsed_Seconds (T) >= Seconds;
   end Has_Elapsed_Seconds;

   ----------------
   -- Is_Running --
   ----------------

   function Is_Running (T : Timer_Type) return Boolean is
   begin
      return T.Running;
   end Is_Running;

   ------------------
   -- Delay_Ticks --
   ------------------

   procedure Delay_Ticks (Ticks : Natural) is
      Start : constant Natural := Current_Ticks;
   begin
      while (Current_Ticks - Start) < Ticks loop
         null;  -- Busy wait
      end loop;
   end Delay_Ticks;

   --------------
   -- Delay_MS --
   --------------

   procedure Delay_MS (MS : Natural) is
      Ticks : constant Natural := (MS * Ticks_Per_Second) / 1000;
   begin
      if Ticks > 0 then
         Delay_Ticks (Ticks);
      end if;
   end Delay_MS;

   -------------------
   -- Set_Countdown --
   -------------------

   procedure Set_Countdown (C : out Countdown; MS : Natural) is
   begin
      C.Start_Tick := Current_Ticks;
      C.Duration := (MS * Ticks_Per_Second) / 1000;
   end Set_Countdown;

   -------------------------
   -- Set_Countdown_Ticks --
   -------------------------

   procedure Set_Countdown_Ticks (C : out Countdown; Ticks : Natural) is
   begin
      C.Start_Tick := Current_Ticks;
      C.Duration := Ticks;
   end Set_Countdown_Ticks;

   -----------------
   -- Has_Expired --
   -----------------

   function Has_Expired (C : Countdown) return Boolean is
      Elapsed : constant Natural := Current_Ticks - C.Start_Tick;
   begin
      return Elapsed >= C.Duration;
   end Has_Expired;

   ------------------
   -- Remaining_MS --
   ------------------

   function Remaining_MS (C : Countdown) return Natural is
      Elapsed : constant Natural := Current_Ticks - C.Start_Tick;
      Remaining : Natural;
   begin
      if Elapsed >= C.Duration then
         return 0;
      else
         Remaining := C.Duration - Elapsed;
         return (Remaining * 1000) / Ticks_Per_Second;
      end if;
   end Remaining_MS;

   ---------------------
   -- Remaining_Ticks --
   ---------------------

   function Remaining_Ticks (C : Countdown) return Natural is
      Elapsed : constant Natural := Current_Ticks - C.Start_Tick;
   begin
      if Elapsed >= C.Duration then
         return 0;
      else
         return C.Duration - Elapsed;
      end if;
   end Remaining_Ticks;

   ---------------------
   -- Reset_Countdown --
   ---------------------

   procedure Reset_Countdown (C : in Out Countdown) is
   begin
      C.Start_Tick := Current_Ticks;
   end Reset_Countdown;

end GNAT.Timer;
