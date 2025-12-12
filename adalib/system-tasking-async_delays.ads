-- System.Tasking.Async_Delays for Z80
-- Asynchronous delay operations

with Ada.Real_Time;

package System.Tasking.Async_Delays is
   pragma Preelaborate;

   -- Delay state for timed operations
   type Delay_Block is limited private;

   -- Initialize a delay block
   procedure Initialize (D : out Delay_Block);

   -- Start a timed delay
   procedure Enqueue_Duration
     (D        : in Out Delay_Block;
      Duration : Duration);

   procedure Enqueue_Time_Span
     (D         : in Out Delay_Block;
      Time_Span : Ada.Real_Time.Time_Span);

   procedure Enqueue_Calendar
     (D : in Out Delay_Block;
      T : Duration);  -- Calendar.Time as Duration

   procedure Enqueue_RT
     (D : in Out Delay_Block;
      T : Ada.Real_Time.Time);

   -- Cancel a pending delay
   procedure Cancel (D : in Out Delay_Block; Cancelled : out Boolean);

   -- Check if delay has expired
   function Timed_Out (D : Delay_Block) return Boolean;

private

   type Delay_Block is limited record
      Expires    : Ada.Real_Time.Time := Ada.Real_Time.Time_First;
      Active     : Boolean := False;
      Timed_Out  : Boolean := False;
   end record;

end System.Tasking.Async_Delays;
