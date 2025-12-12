-- GNAT.Traceback.Symbolic body for Z80
-- Symbolic stack traceback implementation

package body GNAT.Traceback.Symbolic is

   -------------------------
   -- Symbolic_Traceback --
   -------------------------

   function Symbolic_Traceback
     (Traceback : Tracebacks_Array) return String
   is
      pragma Unreferenced (Traceback);
   begin
      -- No debug info available on Z80
      return "[No symbolic traceback available]";
   end Symbolic_Traceback;

   --------------------------------
   -- Symbolic_Traceback_No_Hex --
   --------------------------------

   function Symbolic_Traceback_No_Hex
     (Traceback : Tracebacks_Array) return String
   is
   begin
      return Symbolic_Traceback (Traceback);
   end Symbolic_Traceback_No_Hex;

end GNAT.Traceback.Symbolic;
