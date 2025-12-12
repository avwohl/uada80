-- Ada.Directories for Z80/CP/M
-- Directory and file operations
--
-- Note: CP/M has a flat file system with limited directory operations.
-- This implementation provides what's possible under CP/M constraints.

package Ada.Directories is
   pragma Preelaborate;

   -- File and Directory name maximum lengths
   Max_Filename_Length : constant := 12;  -- 8.3 format
   Max_Path_Length     : constant := 14;  -- Drive + filename (A:FILENAME.EXT)

   -- File types
   type File_Kind is (Directory, Ordinary_File, Special_File);

   -- File size type (16-bit for Z80)
   type File_Size is range 0 .. 65535;

   -- Directory entry for search operations
   type Directory_Entry_Type is private;

   -- Filter for directory search
   type Filter_Type is array (File_Kind) of Boolean;

   -- Exceptions
   Name_Error   : exception;  -- Invalid name
   Use_Error    : exception;  -- Operation not supported
   Status_Error : exception;  -- File not accessible

   ----------------------
   -- Path operations --
   ----------------------

   function Current_Directory return String;
   -- Returns current drive letter (e.g., "A:")

   procedure Set_Directory (Directory : String);
   -- Sets current drive (e.g., "A:" or "B:")

   function Exists (Name : String) return Boolean;
   -- Check if file exists

   function Kind (Name : String) return File_Kind;
   -- Always returns Ordinary_File for existing files (CP/M has no directories)

   function Size (Name : String) return File_Size;
   -- Returns file size in bytes

   -------------------------
   -- Name composition --
   -------------------------

   function Full_Name (Name : String) return String;
   -- Returns full path with drive letter

   function Simple_Name (Name : String) return String;
   -- Returns just the filename portion

   function Containing_Directory (Name : String) return String;
   -- Returns the drive letter portion

   function Extension (Name : String) return String;
   -- Returns the file extension

   function Base_Name (Name : String) return String;
   -- Returns filename without extension

   function Compose
     (Containing_Directory : String := "";
      Name                 : String;
      Extension            : String := "") return String;
   -- Composes a full file path

   -------------------------
   -- File operations --
   -------------------------

   procedure Delete_File (Name : String);
   -- Deletes a file

   procedure Rename (Old_Name, New_Name : String);
   -- Renames a file

   procedure Copy_File (Source_Name, Target_Name : String);
   -- Copies a file

   --------------------------
   -- Directory iteration --
   --------------------------

   procedure Start_Search
     (Search    : in out Search_Type;
      Directory : String;
      Pattern   : String;
      Filter    : Filter_Type := (others => True));
   -- Start a directory search with wildcard pattern

   procedure End_Search (Search : in Out Search_Type);
   -- End a directory search

   function More_Entries (Search : Search_Type) return Boolean;
   -- Check if more entries exist

   procedure Get_Next_Entry
     (Search          : in Out Search_Type;
      Directory_Entry : out Directory_Entry_Type);
   -- Get next matching entry

   -- Directory entry queries
   function Simple_Name (Directory_Entry : Directory_Entry_Type) return String;
   function Full_Name (Directory_Entry : Directory_Entry_Type) return String;
   function Kind (Directory_Entry : Directory_Entry_Type) return File_Kind;
   function Size (Directory_Entry : Directory_Entry_Type) return File_Size;

   -- Search state type
   type Search_Type is limited private;

private

   -- Directory entry storage
   type Directory_Entry_Type is record
      Name      : String (1 .. Max_Filename_Length) := (others => ' ');
      Name_Len  : Natural := 0;
      Drive     : Character := 'A';
      File_Size : Ada.Directories.File_Size := 0;
      Valid     : Boolean := False;
   end record;

   -- Search state for CP/M BDOS calls
   type Search_Type is record
      Pattern    : String (1 .. Max_Filename_Length) := (others => ' ');
      Pat_Len    : Natural := 0;
      Drive      : Character := 'A';
      FCB        : String (1 .. 36) := (others => Character'Val (0));  -- CP/M FCB
      DMA        : String (1 .. 128) := (others => Character'Val (0)); -- DMA buffer
      First_Call : Boolean := True;
      Has_More   : Boolean := False;
      Filter     : Filter_Type := (others => True);
   end record;

end Ada.Directories;
