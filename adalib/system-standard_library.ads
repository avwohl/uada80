-- System.Standard_Library for Z80
-- Interface to standard library initialization

package System.Standard_Library is
   pragma Preelaborate;

   -- Exception message for Ada exceptions
   type Exception_Data is record
      Name : System.Address;
      Msg  : System.Address;
   end record;

   -- Get last exception data
   function Exception_Information return System.Address;

   -- Initialize standard library
   procedure Adainit;

   -- Finalize standard library
   procedure Adafinal;

   -- Program termination
   procedure Exit_Status (Status : Integer);

end System.Standard_Library;
