-- System.Traces body for Z80
-- Execution tracing implementation

with Ada.Text_IO;

package body System.Traces is

   Trace_Flags : array (Trace_Kind) of Boolean := (others => False);

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Kind    : Trace_Kind;
      Message : String)
   is
   begin
      if Trace_On and then Trace_Flags (Kind) then
         case Kind is
            when None =>
               null;
            when Execution =>
               Ada.Text_IO.Put ("[EXEC] ");
            when Task_State =>
               Ada.Text_IO.Put ("[TASK] ");
            when Memory =>
               Ada.Text_IO.Put ("[MEM]  ");
            when Exception_Trace =>
               Ada.Text_IO.Put ("[EXC]  ");
         end case;
         Ada.Text_IO.Put_Line (Message);
      end if;
   end Trace;

   ---------------
   -- Set_Trace --
   ---------------

   procedure Set_Trace (Kind : Trace_Kind; Enable : Boolean) is
   begin
      Trace_Flags (Kind) := Enable;
   end Set_Trace;

end System.Traces;
