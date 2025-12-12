-- Ada.IO_Exceptions for Z80
-- I/O exception definitions
--
-- Provides standard I/O exceptions used by Text_IO and other I/O packages

package Ada.IO_Exceptions is
   pragma Pure;

   -- Raised when an attempt is made to perform an operation on a closed file
   Status_Error : exception;

   -- Raised when an attempt is made to perform an incompatible operation on a file
   Mode_Error : exception;

   -- Raised when an illegal file name is specified
   Name_Error : exception;

   -- Raised when a device or file cannot be used (disk full, write protected, etc.)
   Use_Error : exception;

   -- Raised when a device malfunctions or is unavailable
   Device_Error : exception;

   -- Raised when end of file is reached unexpectedly
   End_Error : exception;

   -- Raised when data cannot be converted to the required type
   Data_Error : exception;

   -- Raised when a line is too long
   Layout_Error : exception;

end Ada.IO_Exceptions;
