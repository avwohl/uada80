-- GNAT.Traceback.Handling for Z80
-- Traceback handling support

with Ada.Exceptions;
with GNAT.Traceback;

package GNAT.Traceback.Handling is

   function Symbolic_Traceback
     (E : Ada.Exceptions.Exception_Occurrence) return String;
   --  Return symbolic traceback for exception

   function Symbolic_Traceback
     (Traceback : GNAT.Traceback.Tracebacks_Array) return String;
   --  Return symbolic traceback for addresses

end GNAT.Traceback.Handling;
