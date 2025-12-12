-- GNAT.Exceptions body for Z80
-- Additional exception utilities implementation

with Ada.Exceptions;

package body GNAT.Exceptions is

   ------------------------
   -- Reraise_Occurrence --
   ------------------------

   procedure Reraise_Occurrence
     (X : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Ada.Exceptions.Reraise_Occurrence (X);
   end Reraise_Occurrence;

   ----------------------------------
   -- Reraise_Occurrence_No_Defer --
   ----------------------------------

   procedure Reraise_Occurrence_No_Defer
     (X : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      -- Same as regular reraise on Z80 (no abort deferral)
      Ada.Exceptions.Reraise_Occurrence (X);
   end Reraise_Occurrence_No_Defer;

   ----------------------------
   -- Raise_Exception_Always --
   ----------------------------

   procedure Raise_Exception_Always
     (E       : Ada.Exceptions.Exception_Id;
      Message : String := "")
   is
   begin
      Ada.Exceptions.Raise_Exception (E, Message);
   end Raise_Exception_Always;

   ---------------------------
   -- Exception_Information --
   ---------------------------

   function Exception_Information
     (X : Ada.Exceptions.Exception_Occurrence) return String
   is
   begin
      return "Exception: " & Ada.Exceptions.Exception_Name (X) &
             " Message: " & Ada.Exceptions.Exception_Message (X);
   end Exception_Information;

end GNAT.Exceptions;
