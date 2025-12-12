-- System.OS_Primitives body for Z80/CP/M
-- OS-level primitive operations implementation

with System.Machine_Code;

package body System.OS_Primitives is

   Tick_Count : Natural := 0;
   pragma Volatile (Tick_Count);

   -----------
   -- Clock --
   -----------

   function Clock return Time_Type is
   begin
      -- Convert tick count to seconds (assuming 50Hz timer)
      return Time_Type (Tick_Count) * Clock_Resolution;
   end Clock;

   -----------------
   -- Timed_Delay --
   -----------------

   procedure Timed_Delay
     (Time : Time_Type;
      Mode : Integer)
   is
      Target   : Time_Type;
      Current  : Time_Type;
      End_Tick : Natural;
   begin
      Current := Clock;

      if Mode = Absolute then
         Target := Time;
      else
         Target := Current + Time;
      end if;

      -- Calculate target tick
      End_Tick := Natural (Target / Clock_Resolution);

      -- Busy wait (simple implementation)
      while Tick_Count < End_Tick loop
         -- Could use HALT instruction to save power
         System.Machine_Code.Asm ("halt", Volatile => True);
      end loop;
   end Timed_Delay;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Tick_Count := 0;
   end Initialize;

   -- Timer tick handler (called from interrupt)
   procedure Timer_Tick is
   begin
      Tick_Count := Tick_Count + 1;
   end Timer_Tick;
   pragma Export (C, Timer_Tick, "ada_os_timer_tick");

end System.OS_Primitives;
