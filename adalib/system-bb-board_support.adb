-- System.BB.Board_Support body for Z80
-- Bare board support implementation

package body System.BB.Board_Support is

   Tick_Count : Natural := 0;
   pragma Volatile (Tick_Count);

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      Tick_Count := 0;
   end Initialize_Board;

   -----------------------
   -- Initialize_Timers --
   -----------------------

   procedure Initialize_Timers is
   begin
      -- Timer initialization is handled by cpmemu
      -- via --mask-interrupt option
      null;
   end Initialize_Timers;

   ----------------------------
   -- Clear_Alarm_Interrupt --
   ----------------------------

   procedure Clear_Alarm_Interrupt is
   begin
      -- Interrupt automatically cleared by Z80
      null;
   end Clear_Alarm_Interrupt;

   ----------------
   -- Read_Clock --
   ----------------

   function Read_Clock return Natural is
   begin
      return Tick_Count;
   end Read_Clock;

   ---------------
   -- Set_Alarm --
   ---------------

   procedure Set_Alarm (Ticks : Natural) is
      pragma Unreferenced (Ticks);
   begin
      -- cpmemu provides regular interrupts; no need to set specific alarm
      null;
   end Set_Alarm;

   ---------------
   -- BDOS_Call --
   ---------------

   procedure BDOS_Call
     (Function_Code : Natural;
      DE_Value      : Natural := 0;
      Result        : out Natural)
   is
      -- BDOS entry point
      BDOS_Entry : constant System.Address := System'To_Address (16#0005#);
      pragma Unreferenced (BDOS_Entry);
   begin
      -- This would be implemented in assembly
      -- LD C, Function_Code
      -- LD DE, DE_Value
      -- CALL 5
      -- Result in A (or HL for some functions)
      pragma Warnings (Off);
      Result := 0;  -- Stub implementation
      pragma Warnings (On);
      null;
   end BDOS_Call;

   -- Internal: called by timer interrupt handler
   procedure Increment_Tick is
   begin
      Tick_Count := Tick_Count + 1;
   end Increment_Tick;
   pragma Export (C, Increment_Tick, "ada_bb_tick");

end System.BB.Board_Support;
