-- System.BB.Time body for Z80
-- Bare board time management implementation

with System.BB.Board_Support;

package body System.BB.Time is

   Current_Time : Time := Time_First;
   pragma Volatile (Current_Time);

   -----------
   -- Clock --
   -----------

   function Clock return Time is
   begin
      return Time (Board_Support.Read_Clock);
   end Clock;

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Time) return Time is
   begin
      return Time (Natural (Left) + Natural (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Time; Right : Time) return Time is
   begin
      if Natural (Left) > Natural (Right) then
         return Time (Natural (Left) - Natural (Right));
      else
         return Time_First;
      end if;
   end "-";

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
   begin
      while Clock < T loop
         -- Could use HALT to save power
         null;
      end loop;
   end Delay_Until;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Current_Time := Time_First;
      Board_Support.Initialize_Timers;
   end Initialize;

   -- Internal: called by timer interrupt
   procedure Tick is
   begin
      Current_Time := Current_Time + 1;
   end Tick;
   pragma Export (C, Tick, "ada_bb_time_tick");

end System.BB.Time;
