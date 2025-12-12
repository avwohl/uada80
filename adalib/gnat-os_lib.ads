-- GNAT.OS_Lib for Z80
-- Operating system interface (CP/M adaptation)

package GNAT.OS_Lib is
   pragma Preelaborate;

   -- File descriptor
   subtype File_Descriptor is Integer;
   Invalid_FD : constant File_Descriptor := -1;
   Standin    : constant File_Descriptor := 0;
   Standout   : constant File_Descriptor := 1;
   Standerr   : constant File_Descriptor := 2;

   -- File mode
   type File_Mode is (Binary, Text);

   -- Time stamp
   type OS_Time is private;
   Invalid_Time : constant OS_Time;

   -- String access
   type String_Access is access all String;
   procedure Free (X : in Out String_Access);

   -- Argument list
   type Argument_List is array (Positive range <>) of String_Access;
   type Argument_List_Access is access all Argument_List;

   -- File operations
   function Open_Read
     (Name  : String;
      Fmode : File_Mode) return File_Descriptor;

   function Open_Read_Write
     (Name  : String;
      Fmode : File_Mode) return File_Descriptor;

   function Create_File
     (Name  : String;
      Fmode : File_Mode) return File_Descriptor;

   function Create_New_File
     (Name  : String;
      Fmode : File_Mode) return File_Descriptor;

   procedure Close (FD : File_Descriptor);

   function Read
     (FD   : File_Descriptor;
      A    : System.Address;
      N    : Integer) return Integer;

   function Write
     (FD   : File_Descriptor;
      A    : System.Address;
      N    : Integer) return Integer;

   -- File queries
   function Is_Regular_File (Name : String) return Boolean;
   function Is_Directory (Name : String) return Boolean;
   function File_Length (Name : String) return Long_Integer;
   function File_Time_Stamp (Name : String) return OS_Time;

   -- File management
   procedure Delete_File (Name : String; Success : out Boolean);
   procedure Rename_File
     (Old_Name : String;
      New_Name : String;
      Success  : out Boolean);
   procedure Copy_File
     (Name     : String;
      Pathname : String;
      Success  : out Boolean;
      Mode     : File_Mode := Text);

   -- Directory operations
   procedure Change_Dir (Dir_Name : String; Success : out Boolean);
   function Get_Current_Dir return String;

   -- Environment
   function Getenv (Name : String) return String_Access;

   -- Process operations
   function Spawn
     (Program_Name : String;
      Args         : Argument_List) return Integer;

   procedure OS_Exit (Status : Integer);
   pragma No_Return (OS_Exit);

   -- Timestamps
   function GM_Year   (Date : OS_Time) return Integer;
   function GM_Month  (Date : OS_Time) return Integer;
   function GM_Day    (Date : OS_Time) return Integer;
   function GM_Hour   (Date : OS_Time) return Integer;
   function GM_Minute (Date : OS_Time) return Integer;
   function GM_Second (Date : OS_Time) return Integer;

private

   type OS_Time is new Long_Integer;
   Invalid_Time : constant OS_Time := -1;

end GNAT.OS_Lib;
