-- Ada.Directories.Information body for Z80/CP/M
-- Extended file information implementation

package body Ada.Directories.Information is

   -- Default time value (CP/M has no timestamps)
   Default_Time : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (1980, 1, 1);

   -------------------
   -- Creation_Time --
   -------------------

   function Creation_Time (Name : String) return Ada.Calendar.Time is
      pragma Unreferenced (Name);
   begin
      -- CP/M does not store creation time
      return Default_Time;
   end Creation_Time;

   function Creation_Time (Directory_Entry : Directory_Entry_Type) return Ada.Calendar.Time is
      pragma Unreferenced (Directory_Entry);
   begin
      return Default_Time;
   end Creation_Time;

   -----------------------
   -- Modification_Time --
   -----------------------

   function Modification_Time (Name : String) return Ada.Calendar.Time is
      pragma Unreferenced (Name);
   begin
      -- CP/M does not store modification time
      return Default_Time;
   end Modification_Time;

   function Modification_Time (Directory_Entry : Directory_Entry_Type) return Ada.Calendar.Time is
      pragma Unreferenced (Directory_Entry);
   begin
      return Default_Time;
   end Modification_Time;

   -----------------
   -- Access_Time --
   -----------------

   function Access_Time (Name : String) return Ada.Calendar.Time is
      pragma Unreferenced (Name);
   begin
      -- CP/M does not store access time
      return Default_Time;
   end Access_Time;

   function Access_Time (Directory_Entry : Directory_Entry_Type) return Ada.Calendar.Time is
      pragma Unreferenced (Directory_Entry);
   begin
      return Default_Time;
   end Access_Time;

   ------------------
   -- Is_Read_Only --
   ------------------

   function Is_Read_Only (Name : String) return Boolean is
   begin
      -- Would check CP/M R/O attribute via BDOS call
      -- For now, assume all files are read-write
      pragma Unreferenced (Name);
      return False;
   end Is_Read_Only;

   --------------------
   -- Is_System_File --
   --------------------

   function Is_System_File (Name : String) return Boolean is
   begin
      -- Would check CP/M SYS attribute
      pragma Unreferenced (Name);
      return False;
   end Is_System_File;

   ----------------
   -- Is_Archive --
   ----------------

   function Is_Archive (Name : String) return Boolean is
   begin
      -- Archive attribute available on CP/M 2.2+
      pragma Unreferenced (Name);
      return False;
   end Is_Archive;

   ---------------
   -- Is_Hidden --
   ---------------

   function Is_Hidden (Name : String) return Boolean is
   begin
      -- Hidden is same as System on CP/M
      return Is_System_File (Name);
   end Is_Hidden;

   -----------
   -- Owner --
   -----------

   function Owner (Name : String) return String is
      pragma Unreferenced (Name);
   begin
      -- CP/M has no file ownership
      return "";
   end Owner;

   -----------
   -- Group --
   -----------

   function Group (Name : String) return String is
      pragma Unreferenced (Name);
   begin
      -- CP/M has no group concept
      return "";
   end Group;

   ------------
   -- Device --
   ------------

   function Device (Name : String) return String is
   begin
      -- Return the drive letter if present
      if Name'Length >= 2 and then Name (Name'First + 1) = ':' then
         return Name (Name'First .. Name'First);
      else
         -- Return current default drive
         return "A";
      end if;
   end Device;

end Ada.Directories.Information;
