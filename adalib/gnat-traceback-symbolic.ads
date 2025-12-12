-- GNAT.Traceback.Symbolic for Z80
-- Symbolic stack traceback

package GNAT.Traceback.Symbolic is
   pragma Preelaborate;

   -- Convert traceback to symbolic representation
   function Symbolic_Traceback
     (Traceback : Tracebacks_Array) return String;

   -- Get symbolic traceback of current exception
   function Symbolic_Traceback_No_Hex
     (Traceback : Tracebacks_Array) return String;

end GNAT.Traceback.Symbolic;
