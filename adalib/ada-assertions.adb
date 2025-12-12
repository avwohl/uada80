-- Ada.Assertions body for Z80
-- Assertion checking implementation

package body Ada.Assertions is

   ------------
   -- Assert --
   ------------

   procedure Assert (Check : Boolean) is
   begin
      if not Check then
         raise Assertion_Error;
      end if;
   end Assert;

   procedure Assert (Check : Boolean; Message : String) is
      pragma Unreferenced (Message);
      -- On Z80 with limited memory, we don't store the message
      -- A full implementation might output to console before raising
   begin
      if not Check then
         raise Assertion_Error;
      end if;
   end Assert;

end Ada.Assertions;
