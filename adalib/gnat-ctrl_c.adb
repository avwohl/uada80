-- GNAT.Ctrl_C body for Z80
-- Ctrl-C interrupt handling implementation

package body GNAT.Ctrl_C is

   Current_Handler : Handler_Type := null;

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Handler (Handler : Handler_Type) is
   begin
      Current_Handler := Handler;
      -- On Z80/CP-M, would need to intercept console input
      -- and check for Ctrl-C (character code 3)
   end Install_Handler;

   -----------------------
   -- Uninstall_Handler --
   -----------------------

   procedure Uninstall_Handler is
   begin
      Current_Handler := null;
   end Uninstall_Handler;

   -- Internal: called when Ctrl-C detected
   procedure Handle_Ctrl_C is
   begin
      if Current_Handler /= null then
         Current_Handler.all;
      end if;
   end Handle_Ctrl_C;
   pragma Export (C, Handle_Ctrl_C, "ada_ctrl_c_handler");

end GNAT.Ctrl_C;
