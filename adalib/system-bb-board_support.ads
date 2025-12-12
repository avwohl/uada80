-- System.BB.Board_Support for Z80
-- Bare board support for Z80/CP-M

package System.BB.Board_Support is
   pragma Preelaborate;

   -- System clock frequency (4 MHz typical Z80)
   Clock_Frequency : constant := 4_000_000;

   -- Timer interrupt period (in clock cycles)
   Timer_Period : constant := 40_000;  -- ~100 Hz at 4 MHz

   -- Initialize board hardware
   procedure Initialize_Board;

   -- Setup the system clock/timer
   procedure Initialize_Timers;

   -- Clear timer interrupt
   procedure Clear_Alarm_Interrupt;

   -- Get time since boot (in timer ticks)
   function Read_Clock return Natural;

   -- Set alarm for next timer interrupt
   procedure Set_Alarm (Ticks : Natural);

   -- CP/M BDOS call
   procedure BDOS_Call
     (Function_Code : Natural;
      DE_Value      : Natural := 0;
      Result        : out Natural);

end System.BB.Board_Support;
