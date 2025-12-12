-- GNAT.Current_Exception for Z80
-- Access to current exception (compatibility)

with Ada.Exceptions;

package GNAT.Current_Exception is
   pragma Preelaborate;

   -- Get current exception name
   function Exception_Name return String;

   -- Get current exception message
   function Exception_Message return String;

   -- Get current exception information
   function Exception_Information return String;

end GNAT.Current_Exception;
