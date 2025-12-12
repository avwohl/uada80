-- System.Assertions body for Z80
-- Assertion support implementation

package body System.Assertions is

   --------------------------
   -- Raise_Assert_Failure --
   --------------------------

   procedure Raise_Assert_Failure (Msg : String) is
   begin
      raise Assert_Failure with Msg;
   end Raise_Assert_Failure;

end System.Assertions;
