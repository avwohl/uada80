-- GNAT.Exceptions for Z80
-- Additional exception information utilities

with Ada.Exceptions;

package GNAT.Exceptions is

   -- Exception occurrence copy
   procedure Reraise_Occurrence
     (X : Ada.Exceptions.Exception_Occurrence);
   pragma No_Return (Reraise_Occurrence);
   --  Reraise exception occurrence

   procedure Reraise_Occurrence_No_Defer
     (X : Ada.Exceptions.Exception_Occurrence);
   pragma No_Return (Reraise_Occurrence_No_Defer);
   --  Reraise without defer (for finalization)

   procedure Raise_Exception_Always
     (E       : Ada.Exceptions.Exception_Id;
      Message : String := "");
   pragma No_Return (Raise_Exception_Always);
   --  Raise exception (always, even for null ID)

   function Exception_Information
     (X : Ada.Exceptions.Exception_Occurrence) return String;
   --  Full exception information

   function Exception_Identity
     (X : Ada.Exceptions.Exception_Occurrence) return Ada.Exceptions.Exception_Id
     renames Ada.Exceptions.Exception_Identity;

end GNAT.Exceptions;
