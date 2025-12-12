-- Ada.Execution_Time for Z80
-- Execution time monitoring

with Ada.Task_Identification;
with Ada.Real_Time;

package Ada.Execution_Time is
   pragma Preelaborate;

   type CPU_Time is private;

   CPU_Time_First : constant CPU_Time;
   CPU_Time_Last  : constant CPU_Time;
   CPU_Time_Unit  : constant := 0.001;  -- 1 millisecond
   CPU_Tick       : constant Ada.Real_Time.Time_Span;

   function Clock
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task)
     return CPU_Time;

   function "+" (Left : CPU_Time; Right : Ada.Real_Time.Time_Span) return CPU_Time;
   function "+" (Left : Ada.Real_Time.Time_Span; Right : CPU_Time) return CPU_Time;
   function "-" (Left : CPU_Time; Right : Ada.Real_Time.Time_Span) return CPU_Time;
   function "-" (Left, Right : CPU_Time) return Ada.Real_Time.Time_Span;

   function "<" (Left, Right : CPU_Time) return Boolean;
   function "<=" (Left, Right : CPU_Time) return Boolean;
   function ">" (Left, Right : CPU_Time) return Boolean;
   function ">=" (Left, Right : CPU_Time) return Boolean;

   procedure Split
     (T  : CPU_Time;
      SC : out Ada.Real_Time.Seconds_Count;
      TS : out Ada.Real_Time.Time_Span);

   function Time_Of
     (SC : Ada.Real_Time.Seconds_Count;
      TS : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero)
     return CPU_Time;

private

   type CPU_Time is new Ada.Real_Time.Time;

   CPU_Time_First : constant CPU_Time := CPU_Time (Ada.Real_Time.Time_First);
   CPU_Time_Last  : constant CPU_Time := CPU_Time (Ada.Real_Time.Time_Last);
   CPU_Tick       : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Tick;

end Ada.Execution_Time;
