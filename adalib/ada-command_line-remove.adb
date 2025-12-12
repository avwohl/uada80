-- Ada.Command_Line.Remove body for Z80
-- Command line argument removal (stub)

package body Ada.Command_Line.Remove is

   ---------------------
   -- Remove_Argument --
   ---------------------

   procedure Remove_Argument (Number : Positive) is
      pragma Unreferenced (Number);
   begin
      -- Cannot modify CP/M command line after startup
      null;
   end Remove_Argument;

   ----------------------
   -- Remove_Arguments --
   ----------------------

   procedure Remove_Arguments (From, To : Positive) is
      pragma Unreferenced (From, To);
   begin
      -- Cannot modify CP/M command line after startup
      null;
   end Remove_Arguments;

end Ada.Command_Line.Remove;
