-- Ada.Command_Line.Response_File for Z80
-- Response file processing

package Ada.Command_Line.Response_File is

   procedure Open (Name : String);
   --  Open response file for reading arguments

   function Is_Open return Boolean;
   --  True if response file is open

   function Argument return String;
   --  Get next argument from response file

   function More_Arguments return Boolean;
   --  True if more arguments available

   procedure Close;
   --  Close response file

end Ada.Command_Line.Response_File;
