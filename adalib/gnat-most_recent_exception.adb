-- GNAT.Most_Recent_Exception body for Z80
-- Most recent exception implementation

with System.Soft_Links;

package body GNAT.Most_Recent_Exception is

   Current_Exception : aliased Ada.Exceptions.Exception_Occurrence;

   ----------------
   -- Occurrence --
   ----------------

   function Occurrence return Ada.Exceptions.Exception_Occurrence is
   begin
      return Current_Exception;
   end Occurrence;

   -----------------------
   -- Occurrence_Access --
   -----------------------

   function Occurrence_Access return Ada.Exceptions.Exception_Occurrence_Access is
   begin
      return Current_Exception'Access;
   end Occurrence_Access;

end GNAT.Most_Recent_Exception;
