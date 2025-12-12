-- Ada.Directories.Information for Z80/CP/M
-- Extended file information
--
-- Note: CP/M provides limited file attributes. Many fields are stubs.

with Ada.Calendar;

package Ada.Directories.Information is
   pragma Preelaborate;

   -- File creation time (not available on CP/M)
   function Creation_Time (Name : String) return Ada.Calendar.Time;

   -- File modification time (not available on CP/M)
   function Modification_Time (Name : String) return Ada.Calendar.Time;

   -- File access time (not available on CP/M)
   function Access_Time (Name : String) return Ada.Calendar.Time;

   -- Directory entry versions of time queries
   function Creation_Time (Directory_Entry : Directory_Entry_Type) return Ada.Calendar.Time;
   function Modification_Time (Directory_Entry : Directory_Entry_Type) return Ada.Calendar.Time;
   function Access_Time (Directory_Entry : Directory_Entry_Type) return Ada.Calendar.Time;

   -- File attributes (CP/M specific)
   -- These use CP/M file attributes from the directory entry

   function Is_Read_Only (Name : String) return Boolean;
   -- Returns True if file has R/O attribute set

   function Is_System_File (Name : String) return Boolean;
   -- Returns True if file has SYS attribute set

   function Is_Archive (Name : String) return Boolean;
   -- Returns True if file has Archive attribute set (CP/M 2.2+)

   function Is_Hidden (Name : String) return Boolean;
   -- Same as Is_System_File on CP/M

   -- Identity information (not meaningful on CP/M)
   function Owner (Name : String) return String;
   -- Returns empty string (CP/M has no ownership concept)

   function Group (Name : String) return String;
   -- Returns empty string

   -- Device information
   function Device (Name : String) return String;
   -- Returns the drive letter

end Ada.Directories.Information;
