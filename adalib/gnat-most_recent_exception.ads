-- GNAT.Most_Recent_Exception for Z80
-- Access to most recent exception

with Ada.Exceptions;

package GNAT.Most_Recent_Exception is

   function Occurrence return Ada.Exceptions.Exception_Occurrence;
   --  Return most recent exception occurrence for current task

   function Occurrence_Access return Ada.Exceptions.Exception_Occurrence_Access;
   --  Return access to most recent exception

end GNAT.Most_Recent_Exception;
