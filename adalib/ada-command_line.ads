-- Ada.Command_Line for Z80/CP/M
-- Provides access to command line arguments
--
-- On CP/M, the command line is stored at 0x0080 (128 bytes max)
-- First byte is length, followed by the command tail

package Ada.Command_Line is
   pragma Preelaborate;

   -- Return the number of arguments (excluding program name)
   function Argument_Count return Natural;

   -- Return argument N (1 = first argument after program name)
   function Argument (Number : Positive) return String;

   -- Return the program name (command name)
   function Command_Name return String;

   -- Exit status codes
   type Exit_Status is range 0 .. 255;

   Success : constant Exit_Status := 0;
   Failure : constant Exit_Status := 1;

   -- Set the exit status for program termination
   procedure Set_Exit_Status (Code : Exit_Status);

end Ada.Command_Line;
