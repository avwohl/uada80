-- Ada.Dispatching.Round_Robin for Z80
-- Round-robin dispatching within a priority (Ada 2005)

with Ada.Real_Time;

package Ada.Dispatching.Round_Robin is
   pragma Preelaborate;

   Default_Quantum : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Milliseconds (10);

   -- Set quantum for a priority
   procedure Set_Quantum
     (Pri     : System.Priority;
      Quantum : Ada.Real_Time.Time_Span);

   -- Set quantum for a range of priorities
   procedure Set_Quantum
     (Low, High : System.Priority;
      Quantum   : Ada.Real_Time.Time_Span);

   -- Query quantum
   function Actual_Quantum (Pri : System.Priority)
     return Ada.Real_Time.Time_Span;

   function Is_Round_Robin (Pri : System.Priority) return Boolean;

end Ada.Dispatching.Round_Robin;
