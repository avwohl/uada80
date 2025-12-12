-- GNAT.Exception_Traces body for Z80
-- Exception trace output control implementation

with Ada.Text_IO;
with Ada.Exceptions;

package body GNAT.Exception_Traces is

   Current_Mode    : Trace_Kind := Unhandled_Raise;
   Tracing_Enabled : Boolean := False;
   Custom_Handler  : Exception_Action := null;

   --------------
   -- Trace_On --
   --------------

   procedure Trace_On (Kind : Trace_Kind) is
   begin
      Current_Mode := Kind;
      Tracing_Enabled := True;
   end Trace_On;

   ---------------
   -- Trace_Off --
   ---------------

   procedure Trace_Off is
   begin
      Tracing_Enabled := False;
   end Trace_Off;

   -----------------------
   -- Set_Trace_Handler --
   -----------------------

   procedure Set_Trace_Handler (Handler : Exception_Action) is
   begin
      Custom_Handler := Handler;
   end Set_Trace_Handler;

   -- Internal: called when exception is raised
   procedure Trace_Exception (E : Exception_Occurrence) is
   begin
      if not Tracing_Enabled then
         return;
      end if;

      if Custom_Handler /= null then
         Custom_Handler (E);
      else
         Ada.Text_IO.Put_Line ("Exception: " &
           Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
      end if;
   end Trace_Exception;

end GNAT.Exception_Traces;
