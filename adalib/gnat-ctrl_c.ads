-- GNAT.Ctrl_C for Z80
-- Ctrl-C interrupt handling

package GNAT.Ctrl_C is
   pragma Preelaborate;

   -- Handler type
   type Handler_Type is access procedure;

   -- Install Ctrl-C handler
   procedure Install_Handler (Handler : Handler_Type);

   -- Uninstall handler (restore default)
   procedure Uninstall_Handler;

end GNAT.Ctrl_C;
