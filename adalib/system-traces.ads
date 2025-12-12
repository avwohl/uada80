-- System.Traces for Z80
-- Execution tracing support

package System.Traces is
   pragma Preelaborate;

   -- Trace categories
   type Trace_Kind is
     (None,
      Execution,
      Task_State,
      Memory,
      Exception_Trace);

   -- Enable/disable tracing
   Trace_On : Boolean := False;

   procedure Trace
     (Kind    : Trace_Kind;
      Message : String);
   --  Output trace message if tracing enabled

   procedure Set_Trace (Kind : Trace_Kind; Enable : Boolean);
   --  Enable/disable specific trace category

end System.Traces;
