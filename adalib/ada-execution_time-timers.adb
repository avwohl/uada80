-- Ada.Execution_Time.Timers body for Z80
-- Execution time timers implementation

package body Ada.Execution_Time.Timers is

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (TM      : in Out Timer;
      In_Time : Ada.Real_Time.Time_Span;
      Handler : Timer_Handler)
   is
      Current : constant CPU_Time := Clock (TM.T.all);
   begin
      TM.Handler := Handler;
      TM.Expires := Current + In_Time;
      TM.Active := Handler /= null;
   end Set_Handler;

   procedure Set_Handler
     (TM      : in Out Timer;
      At_Time : CPU_Time;
      Handler : Timer_Handler)
   is
   begin
      TM.Handler := Handler;
      TM.Expires := At_Time;
      TM.Active := Handler /= null;
   end Set_Handler;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler (TM : Timer) return Timer_Handler is
   begin
      return TM.Handler;
   end Current_Handler;

   --------------------
   -- Time_Remaining --
   --------------------

   function Time_Remaining (TM : Timer) return Ada.Real_Time.Time_Span is
      Current : constant CPU_Time := Clock (TM.T.all);
   begin
      if not TM.Active or else Current >= TM.Expires then
         return Ada.Real_Time.Time_Span_Zero;
      else
         return TM.Expires - Current;
      end if;
   end Time_Remaining;

   --------------------
   -- Cancel_Handler --
   --------------------

   procedure Cancel_Handler
     (TM        : in Out Timer;
      Cancelled : out Boolean)
   is
   begin
      Cancelled := TM.Active;
      TM.Handler := null;
      TM.Active := False;
   end Cancel_Handler;

end Ada.Execution_Time.Timers;
