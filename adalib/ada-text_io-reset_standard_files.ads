-- Ada.Text_IO.Reset_Standard_Files for Z80
-- Reset standard I/O files

package Ada.Text_IO.Reset_Standard_Files is

   procedure Reset_Standard_Input;
   --  Reset standard input to initial state

   procedure Reset_Standard_Output;
   --  Reset standard output to initial state

   procedure Reset_Standard_Error;
   --  Reset standard error to initial state

   procedure Reset_All;
   --  Reset all standard files

end Ada.Text_IO.Reset_Standard_Files;
