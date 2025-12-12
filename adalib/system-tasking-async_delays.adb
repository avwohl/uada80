-- System.Tasking.Async_Delays body for Z80
-- Asynchronous delay operations implementation

package body System.Tasking.Async_Delays is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (D : out Delay_Block) is
   begin
      D.Expires := Ada.Real_Time.Time_First;
      D.Active := False;
      D.Timed_Out := False;
   end Initialize;

   ----------------------
   -- Enqueue_Duration --
   ----------------------

   procedure Enqueue_Duration
     (D        : in Out Delay_Block;
      Duration : Standard.Duration)
   is
      use Ada.Real_Time;
   begin
      if Duration > 0.0 then
         D.Expires := Clock + To_Time_Span (Duration);
         D.Active := True;
         D.Timed_Out := False;
      else
         D.Active := False;
         D.Timed_Out := True;
      end if;
   end Enqueue_Duration;

   ------------------------
   -- Enqueue_Time_Span --
   ------------------------

   procedure Enqueue_Time_Span
     (D         : in Out Delay_Block;
      Time_Span : Ada.Real_Time.Time_Span)
   is
      use Ada.Real_Time;
   begin
      if Time_Span > Time_Span_Zero then
         D.Expires := Clock + Time_Span;
         D.Active := True;
         D.Timed_Out := False;
      else
         D.Active := False;
         D.Timed_Out := True;
      end if;
   end Enqueue_Time_Span;

   ----------------------
   -- Enqueue_Calendar --
   ----------------------

   procedure Enqueue_Calendar
     (D : in Out Delay_Block;
      T : Duration)
   is
      use Ada.Real_Time;
   begin
      -- Convert calendar time to real time
      -- For Z80, we just use the duration from epoch
      D.Expires := Time_Of (Seconds_Count (T), Time_Span_Zero);
      D.Active := True;
      D.Timed_Out := False;
   end Enqueue_Calendar;

   ----------------
   -- Enqueue_RT --
   ----------------

   procedure Enqueue_RT
     (D : in Out Delay_Block;
      T : Ada.Real_Time.Time)
   is
   begin
      D.Expires := T;
      D.Active := True;
      D.Timed_Out := False;
   end Enqueue_RT;

   ------------
   -- Cancel --
   ------------

   procedure Cancel (D : in Out Delay_Block; Cancelled : out Boolean) is
   begin
      Cancelled := D.Active;
      D.Active := False;
   end Cancel;

   ---------------
   -- Timed_Out --
   ---------------

   function Timed_Out (D : Delay_Block) return Boolean is
      use Ada.Real_Time;
   begin
      if D.Timed_Out then
         return True;
      elsif D.Active and then Clock >= D.Expires then
         return True;
      else
         return False;
      end if;
   end Timed_Out;

end System.Tasking.Async_Delays;
