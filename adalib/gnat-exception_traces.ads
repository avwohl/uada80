-- GNAT.Exception_Traces for Z80
-- Exception trace output control

package GNAT.Exception_Traces is
   pragma Preelaborate;

   -- Trace modes
   type Trace_Kind is (Every_Raise, Unhandled_Raise);

   -- Enable/disable tracing
   procedure Trace_On (Kind : Trace_Kind);
   procedure Trace_Off;

   -- Custom trace handler
   type Exception_Action is access procedure (E : Exception_Occurrence);
   procedure Set_Trace_Handler (Handler : Exception_Action);

end GNAT.Exception_Traces;
