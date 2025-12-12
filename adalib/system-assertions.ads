-- System.Assertions for Z80
-- Assertion support
--
-- Provides runtime assertion checking support

package System.Assertions is
   pragma Pure;

   -- Exception raised when an assertion fails
   Assert_Failure : exception;

   -- Raise Assert_Failure with a message
   procedure Raise_Assert_Failure (Msg : String);
   pragma No_Return (Raise_Assert_Failure);

end System.Assertions;
