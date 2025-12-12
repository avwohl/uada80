-- System.BB.Parameters for Z80
-- Bare board parameters

package System.BB.Parameters is
   pragma Pure;

   -- Number of hardware interrupt vectors
   Interrupt_Vectors : constant := 8;

   -- Z80 has RST 0, RST 8, RST 10h, RST 18h, RST 20h, RST 28h, RST 30h, RST 38h
   -- Plus NMI at 0066h

   -- Clock parameters
   Clock_Frequency : constant := 4_000_000;  -- 4 MHz

   -- Tick frequency for scheduler
   Ticks_Per_Second : constant := 100;  -- 100 Hz tick rate

   -- Maximum number of tasks
   Max_Tasks : constant := 8;

   -- Stack parameters
   Default_Stack_Size    : constant := 256;
   Minimum_Stack_Size    : constant := 128;
   Environment_Task_Size : constant := 512;

   -- Priority parameters
   Max_Priority : constant := 15;
   Default_Priority : constant := 7;

end System.BB.Parameters;
