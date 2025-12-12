-- Ada.Text_IO.Reset_Standard_Files body for Z80
-- Reset standard I/O files implementation

package body Ada.Text_IO.Reset_Standard_Files is

   --------------------------
   -- Reset_Standard_Input --
   --------------------------

   procedure Reset_Standard_Input is
   begin
      -- On CP/M, console is always ready
      null;
   end Reset_Standard_Input;

   ---------------------------
   -- Reset_Standard_Output --
   ---------------------------

   procedure Reset_Standard_Output is
   begin
      -- Flush any pending output
      Flush (Standard_Output);
   end Reset_Standard_Output;

   --------------------------
   -- Reset_Standard_Error --
   --------------------------

   procedure Reset_Standard_Error is
   begin
      Flush (Standard_Error);
   end Reset_Standard_Error;

   ---------------
   -- Reset_All --
   ---------------

   procedure Reset_All is
   begin
      Reset_Standard_Input;
      Reset_Standard_Output;
      Reset_Standard_Error;
   end Reset_All;

end Ada.Text_IO.Reset_Standard_Files;
