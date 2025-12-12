-- System.IO_Status for Z80/CP/M
-- I/O operation status codes

package System.IO_Status is
   pragma Pure;

   -- I/O operation result
   type IO_Result is (
      Success,
      End_Of_File,
      Device_Error,
      Name_Error,
      Use_Error,
      Mode_Error,
      Status_Error,
      Data_Error,
      Layout_Error);

   -- CP/M BDOS return codes
   type BDOS_Result is mod 256;

   BDOS_Success    : constant BDOS_Result := 0;
   BDOS_Error      : constant BDOS_Result := 255;
   BDOS_Not_Found  : constant BDOS_Result := 255;
   BDOS_EOF        : constant BDOS_Result := 26;   -- Ctrl-Z

   -- Convert BDOS result to IO_Result
   function To_IO_Result (BDOS : BDOS_Result) return IO_Result;

   -- Get error message for result
   function Error_Message (Result : IO_Result) return String;

end System.IO_Status;
