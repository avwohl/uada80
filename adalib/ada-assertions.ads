-- Ada.Assertions for Z80
-- Assertion checking support
--
-- Provides Assert procedure and Assertion_Error exception

package Ada.Assertions is
   pragma Pure;

   Assertion_Error : exception;

   procedure Assert (Check : Boolean);
   -- Raises Assertion_Error if Check is False

   procedure Assert (Check : Boolean; Message : String);
   -- Raises Assertion_Error with Message if Check is False

end Ada.Assertions;
