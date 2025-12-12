-- Ada.Exceptions specification for Z80
-- Exception handling utilities
--
-- This package provides the standard Ada.Exceptions functionality
-- for retrieving information about raised exceptions.

package Ada.Exceptions is
   pragma Preelaborate;

   -- Exception_Id identifies an exception type
   type Exception_Id is private;
   Null_Id : constant Exception_Id;

   -- Exception_Occurrence holds information about a raised exception
   type Exception_Occurrence is limited private;
   type Exception_Occurrence_Access is access all Exception_Occurrence;

   Null_Occurrence : constant Exception_Occurrence;

   -- Get the identity of an exception occurrence
   function Exception_Identity (X : Exception_Occurrence) return Exception_Id;

   -- Get the name of an exception
   function Exception_Name (Id : Exception_Id) return String;
   function Exception_Name (X : Exception_Occurrence) return String;

   -- Wide string versions (same as String on Z80)
   function Wide_Exception_Name (Id : Exception_Id) return Wide_String;
   function Wide_Exception_Name (X : Exception_Occurrence) return Wide_String;

   function Wide_Wide_Exception_Name (Id : Exception_Id) return Wide_Wide_String;
   function Wide_Wide_Exception_Name (X : Exception_Occurrence) return Wide_Wide_String;

   -- Get the message associated with an exception occurrence
   function Exception_Message (X : Exception_Occurrence) return String;

   -- Get full diagnostic information
   function Exception_Information (X : Exception_Occurrence) return String;

   -- Raise an exception with message
   procedure Raise_Exception (E : Exception_Id; Message : String := "");
   pragma No_Return (Raise_Exception);

   procedure Raise_Exception_Always (E : Exception_Id; Message : String := "");
   pragma No_Return (Raise_Exception_Always);

   -- Re-raise the current exception
   procedure Reraise_Occurrence (X : Exception_Occurrence);
   pragma No_Return (Reraise_Occurrence);

   procedure Reraise_Occurrence_Always (X : Exception_Occurrence);
   pragma No_Return (Reraise_Occurrence_Always);

   procedure Reraise_Occurrence_No_Defer (X : Exception_Occurrence);
   pragma No_Return (Reraise_Occurrence_No_Defer);

   -- Save exception occurrence for later use
   procedure Save_Occurrence
     (Target : out Exception_Occurrence;
      Source : Exception_Occurrence);

   function Save_Occurrence
     (Source : Exception_Occurrence) return Exception_Occurrence_Access;

private

   -- Exception_Id is just the exception's unique identifier
   type Exception_Id is new Integer range 0 .. 255;
   Null_Id : constant Exception_Id := 0;

   -- Exception_Occurrence holds the occurrence details
   -- Kept small for Z80 memory constraints
   type Exception_Occurrence is record
      Id      : Exception_Id := 0;
      Msg_Ptr : System.Address := System.Null_Address;
      Msg_Len : Natural := 0;
   end record;

   Null_Occurrence : constant Exception_Occurrence :=
     (Id => 0, Msg_Ptr => System.Null_Address, Msg_Len => 0);

end Ada.Exceptions;
