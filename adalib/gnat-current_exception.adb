-- GNAT.Current_Exception body for Z80
-- Access to current exception (compatibility) implementation

with Ada.Exceptions;

package body GNAT.Current_Exception is

   --------------------
   -- Exception_Name --
   --------------------

   function Exception_Name return String is
   begin
      -- Would need to access current exception from runtime
      return "Unknown_Exception";
   end Exception_Name;

   -----------------------
   -- Exception_Message --
   -----------------------

   function Exception_Message return String is
   begin
      return "";
   end Exception_Message;

   ---------------------------
   -- Exception_Information --
   ---------------------------

   function Exception_Information return String is
   begin
      return Exception_Name & ": " & Exception_Message;
   end Exception_Information;

end GNAT.Current_Exception;
