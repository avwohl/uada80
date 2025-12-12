-- GNAT.Traceback.Handling body for Z80
-- Traceback handling implementation

package body GNAT.Traceback.Handling is

   -------------------------
   -- Symbolic_Traceback --
   -------------------------

   function Symbolic_Traceback
     (E : Ada.Exceptions.Exception_Occurrence) return String
   is
      pragma Unreferenced (E);
   begin
      return "[No symbolic traceback - Z80 target]";
   end Symbolic_Traceback;

   -------------------------
   -- Symbolic_Traceback --
   -------------------------

   function Symbolic_Traceback
     (Traceback : GNAT.Traceback.Tracebacks_Array) return String
   is
      pragma Unreferenced (Traceback);
   begin
      return "[No symbolic traceback - Z80 target]";
   end Symbolic_Traceback;

end GNAT.Traceback.Handling;
