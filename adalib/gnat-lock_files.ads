-- GNAT.Lock_Files for Z80
-- File locking utilities

package GNAT.Lock_Files is
   pragma Preelaborate;

   -- Lock directory name
   subtype Path_Name is String;

   -- Create lock file
   procedure Lock_File
     (Directory      : Path_Name;
      Lock_File_Name : Path_Name;
      Wait           : Duration := 1.0;
      Retries        : Natural := Natural'Last);

   -- Create lock with blocking behavior
   procedure Lock_File
     (Lock_File_Name : Path_Name;
      Wait           : Duration := 1.0;
      Retries        : Natural := Natural'Last);

   -- Release lock
   procedure Unlock_File (Lock_File_Name : Path_Name);
   procedure Unlock_File
     (Directory      : Path_Name;
      Lock_File_Name : Path_Name);

   -- Exceptions
   Lock_Error : exception;

end GNAT.Lock_Files;
