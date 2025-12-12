-- Ada.Execution_Time body for Z80
-- Execution time monitoring implementation

with System.Tasking;

package body Ada.Execution_Time is

   -- Per-task CPU time (Max_Tasks = 8)
   Task_CPU_Times : array (1 .. 8) of Ada.Real_Time.Time :=
     (others => Ada.Real_Time.Time_First);

   -----------
   -- Clock --
   -----------

   function Clock
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task)
     return CPU_Time
   is
      Index : constant Natural :=
        Natural (System.Tasking.To_Task_Id (T));
   begin
      if Index in Task_CPU_Times'Range then
         return CPU_Time (Task_CPU_Times (Index));
      else
         -- Fallback to real time for environment task
         return CPU_Time (Ada.Real_Time.Clock);
      end if;
   end Clock;

   ---------
   -- "+" --
   ---------

   function "+" (Left : CPU_Time; Right : Ada.Real_Time.Time_Span) return CPU_Time is
   begin
      return CPU_Time (Ada.Real_Time.Time (Left) + Right);
   end "+";

   function "+" (Left : Ada.Real_Time.Time_Span; Right : CPU_Time) return CPU_Time is
   begin
      return CPU_Time (Left + Ada.Real_Time.Time (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : CPU_Time; Right : Ada.Real_Time.Time_Span) return CPU_Time is
   begin
      return CPU_Time (Ada.Real_Time.Time (Left) - Right);
   end "-";

   function "-" (Left, Right : CPU_Time) return Ada.Real_Time.Time_Span is
   begin
      return Ada.Real_Time.Time (Left) - Ada.Real_Time.Time (Right);
   end "-";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : CPU_Time) return Boolean is
   begin
      return Ada.Real_Time.Time (Left) < Ada.Real_Time.Time (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : CPU_Time) return Boolean is
   begin
      return Ada.Real_Time.Time (Left) <= Ada.Real_Time.Time (Right);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : CPU_Time) return Boolean is
   begin
      return Ada.Real_Time.Time (Left) > Ada.Real_Time.Time (Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : CPU_Time) return Boolean is
   begin
      return Ada.Real_Time.Time (Left) >= Ada.Real_Time.Time (Right);
   end ">=";

   -----------
   -- Split --
   -----------

   procedure Split
     (T  : CPU_Time;
      SC : out Ada.Real_Time.Seconds_Count;
      TS : out Ada.Real_Time.Time_Span)
   is
   begin
      Ada.Real_Time.Split (Ada.Real_Time.Time (T), SC, TS);
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (SC : Ada.Real_Time.Seconds_Count;
      TS : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero)
     return CPU_Time
   is
   begin
      return CPU_Time (Ada.Real_Time.Time_Of (SC, TS));
   end Time_Of;

   -- Internal: called by scheduler to update CPU time
   procedure Add_CPU_Time
     (T      : System.Tasking.Task_Id;
      Amount : Ada.Real_Time.Time_Span)
   is
      Index : constant Natural := Natural (T);
   begin
      if Index in Task_CPU_Times'Range then
         Task_CPU_Times (Index) := Task_CPU_Times (Index) + Amount;
      end if;
   end Add_CPU_Time;

end Ada.Execution_Time;
