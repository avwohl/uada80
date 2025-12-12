-- GNAT.Lock_Files body for Z80
-- File locking utilities implementation

with GNAT.OS_Lib;

package body GNAT.Lock_Files is

   ---------------
   -- Lock_File --
   ---------------

   procedure Lock_File
     (Directory      : Path_Name;
      Lock_File_Name : Path_Name;
      Wait           : Duration := 1.0;
      Retries        : Natural := Natural'Last)
   is
      pragma Unreferenced (Wait);
      Full_Name : constant String := Directory & Lock_File_Name;
      FD : GNAT.OS_Lib.File_Descriptor;
      Tries : Natural := 0;
   begin
      loop
         FD := GNAT.OS_Lib.Create_New_File (Full_Name, GNAT.OS_Lib.Binary);
         if FD /= GNAT.OS_Lib.Invalid_FD then
            GNAT.OS_Lib.Close (FD);
            return;
         end if;

         Tries := Tries + 1;
         if Tries > Retries then
            raise Lock_Error with "Cannot create lock file: " & Full_Name;
         end if;

         -- Wait before retry (simplified - no actual delay on Z80)
         null;
      end loop;
   end Lock_File;

   procedure Lock_File
     (Lock_File_Name : Path_Name;
      Wait           : Duration := 1.0;
      Retries        : Natural := Natural'Last)
   is
   begin
      Lock_File ("", Lock_File_Name, Wait, Retries);
   end Lock_File;

   -----------------
   -- Unlock_File --
   -----------------

   procedure Unlock_File (Lock_File_Name : Path_Name) is
      Success : Boolean;
   begin
      GNAT.OS_Lib.Delete_File (Lock_File_Name, Success);
      if not Success then
         raise Lock_Error with "Cannot delete lock file: " & Lock_File_Name;
      end if;
   end Unlock_File;

   procedure Unlock_File
     (Directory      : Path_Name;
      Lock_File_Name : Path_Name)
   is
   begin
      Unlock_File (Directory & Lock_File_Name);
   end Unlock_File;

end GNAT.Lock_Files;
