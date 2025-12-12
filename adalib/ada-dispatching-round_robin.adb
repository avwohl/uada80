-- Ada.Dispatching.Round_Robin body for Z80
-- Round-robin dispatching implementation

package body Ada.Dispatching.Round_Robin is

   -- Quantum per priority level (16 levels)
   Priority_Quantum : array (System.Priority) of Ada.Real_Time.Time_Span :=
     (others => Default_Quantum);

   Round_Robin_Enabled : array (System.Priority) of Boolean :=
     (others => False);

   -----------------
   -- Set_Quantum --
   -----------------

   procedure Set_Quantum
     (Pri     : System.Priority;
      Quantum : Ada.Real_Time.Time_Span)
   is
   begin
      Priority_Quantum (Pri) := Quantum;
      Round_Robin_Enabled (Pri) := True;
   end Set_Quantum;

   procedure Set_Quantum
     (Low, High : System.Priority;
      Quantum   : Ada.Real_Time.Time_Span)
   is
   begin
      for P in Low .. High loop
         Priority_Quantum (P) := Quantum;
         Round_Robin_Enabled (P) := True;
      end loop;
   end Set_Quantum;

   --------------------
   -- Actual_Quantum --
   --------------------

   function Actual_Quantum (Pri : System.Priority)
     return Ada.Real_Time.Time_Span
   is
   begin
      return Priority_Quantum (Pri);
   end Actual_Quantum;

   --------------------
   -- Is_Round_Robin --
   --------------------

   function Is_Round_Robin (Pri : System.Priority) return Boolean is
   begin
      return Round_Robin_Enabled (Pri);
   end Is_Round_Robin;

end Ada.Dispatching.Round_Robin;
