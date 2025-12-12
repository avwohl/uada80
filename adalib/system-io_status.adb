-- System.IO_Status body for Z80/CP/M
-- I/O operation status codes

package body System.IO_Status is

   ------------------
   -- To_IO_Result --
   ------------------

   function To_IO_Result (BDOS : BDOS_Result) return IO_Result is
   begin
      case BDOS is
         when BDOS_Success =>
            return Success;
         when BDOS_EOF =>
            return End_Of_File;
         when BDOS_Not_Found =>
            return Name_Error;
         when others =>
            return Device_Error;
      end case;
   end To_IO_Result;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (Result : IO_Result) return String is
   begin
      case Result is
         when Success =>
            return "Success";
         when End_Of_File =>
            return "End of file";
         when Device_Error =>
            return "Device error";
         when Name_Error =>
            return "File not found";
         when Use_Error =>
            return "Invalid operation";
         when Mode_Error =>
            return "Invalid mode";
         when Status_Error =>
            return "File not open";
         when Data_Error =>
            return "Invalid data";
         when Layout_Error =>
            return "Layout error";
      end case;
   end Error_Message;

end System.IO_Status;
