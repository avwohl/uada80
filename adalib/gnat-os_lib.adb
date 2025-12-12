-- GNAT.OS_Lib body for Z80
-- Operating system interface (CP/M adaptation)

with Ada.Unchecked_Deallocation;
with System.CRTL;

package body GNAT.OS_Lib is

   procedure Dealloc is new Ada.Unchecked_Deallocation
     (String, String_Access);

   ----------
   -- Free --
   ----------

   procedure Free (X : in Out String_Access) is
   begin
      Dealloc (X);
   end Free;

   ---------------
   -- Open_Read --
   ---------------

   function Open_Read
     (Name  : String;
      Fmode : File_Mode) return File_Descriptor
   is
      pragma Unreferenced (Fmode);
   begin
      return File_Descriptor (System.CRTL.Open_Read (Name));
   end Open_Read;

   ---------------------
   -- Open_Read_Write --
   ---------------------

   function Open_Read_Write
     (Name  : String;
      Fmode : File_Mode) return File_Descriptor
   is
      pragma Unreferenced (Fmode);
   begin
      return File_Descriptor (System.CRTL.Open_Read_Write (Name));
   end Open_Read_Write;

   -----------------
   -- Create_File --
   -----------------

   function Create_File
     (Name  : String;
      Fmode : File_Mode) return File_Descriptor
   is
      pragma Unreferenced (Fmode);
   begin
      return File_Descriptor (System.CRTL.Create (Name));
   end Create_File;

   ---------------------
   -- Create_New_File --
   ---------------------

   function Create_New_File
     (Name  : String;
      Fmode : File_Mode) return File_Descriptor
   is
   begin
      -- CP/M doesn't distinguish; just create
      return Create_File (Name, Fmode);
   end Create_New_File;

   -----------
   -- Close --
   -----------

   procedure Close (FD : File_Descriptor) is
   begin
      System.CRTL.Close (Integer (FD));
   end Close;

   ----------
   -- Read --
   ----------

   function Read
     (FD   : File_Descriptor;
      A    : System.Address;
      N    : Integer) return Integer
   is
   begin
      return System.CRTL.Read (Integer (FD), A, N);
   end Read;

   -----------
   -- Write --
   -----------

   function Write
     (FD   : File_Descriptor;
      A    : System.Address;
      N    : Integer) return Integer
   is
   begin
      return System.CRTL.Write (Integer (FD), A, N);
   end Write;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Name : String) return Boolean is
      FD : File_Descriptor;
   begin
      FD := Open_Read (Name, Binary);
      if FD /= Invalid_FD then
         Close (FD);
         return True;
      end if;
      return False;
   end Is_Regular_File;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (Name : String) return Boolean is
      pragma Unreferenced (Name);
   begin
      -- CP/M doesn't have directories in the same sense
      return False;
   end Is_Directory;

   -----------------
   -- File_Length --
   -----------------

   function File_Length (Name : String) return Long_Integer is
      pragma Unreferenced (Name);
   begin
      -- Would need to read file to determine length
      return 0;
   end File_Length;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp (Name : String) return OS_Time is
      pragma Unreferenced (Name);
   begin
      -- CP/M 2.2 doesn't have timestamps
      return Invalid_Time;
   end File_Time_Stamp;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File (Name : String; Success : out Boolean) is
   begin
      Success := System.CRTL.Unlink (Name) = 0;
   end Delete_File;

   -----------------
   -- Rename_File --
   -----------------

   procedure Rename_File
     (Old_Name : String;
      New_Name : String;
      Success  : out Boolean)
   is
   begin
      Success := System.CRTL.Rename (Old_Name, New_Name) = 0;
   end Rename_File;

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File
     (Name     : String;
      Pathname : String;
      Success  : out Boolean;
      Mode     : File_Mode := Text)
   is
      pragma Unreferenced (Mode);
      Src, Dst : File_Descriptor;
      Buffer   : String (1 .. 128);
      N        : Integer;
   begin
      Success := False;
      Src := Open_Read (Name, Binary);
      if Src = Invalid_FD then
         return;
      end if;

      Dst := Create_File (Pathname, Binary);
      if Dst = Invalid_FD then
         Close (Src);
         return;
      end if;

      loop
         N := Read (Src, Buffer'Address, Buffer'Length);
         exit when N <= 0;
         if Write (Dst, Buffer'Address, N) /= N then
            Close (Src);
            Close (Dst);
            return;
         end if;
      end loop;

      Close (Src);
      Close (Dst);
      Success := True;
   end Copy_File;

   ----------------
   -- Change_Dir --
   ----------------

   procedure Change_Dir (Dir_Name : String; Success : out Boolean) is
      pragma Unreferenced (Dir_Name);
   begin
      -- CP/M uses drive letters, not directory paths
      Success := False;
   end Change_Dir;

   ---------------------
   -- Get_Current_Dir --
   ---------------------

   function Get_Current_Dir return String is
   begin
      return "A:";  -- Default CP/M drive
   end Get_Current_Dir;

   ------------
   -- Getenv --
   ------------

   function Getenv (Name : String) return String_Access is
      pragma Unreferenced (Name);
   begin
      -- CP/M doesn't have environment variables
      return null;
   end Getenv;

   -----------
   -- Spawn --
   -----------

   function Spawn
     (Program_Name : String;
      Args         : Argument_List) return Integer
   is
      pragma Unreferenced (Program_Name, Args);
   begin
      -- Would need to load and execute COM file
      return -1;
   end Spawn;

   -------------
   -- OS_Exit --
   -------------

   procedure OS_Exit (Status : Integer) is
      pragma Unreferenced (Status);
   begin
      -- Return to CP/M
      System.CRTL.Exit_Program (0);
   end OS_Exit;

   -- Time functions (CP/M 2.2 doesn't have time)

   function GM_Year   (Date : OS_Time) return Integer is
      pragma Unreferenced (Date);
   begin
      return 1980;
   end GM_Year;

   function GM_Month  (Date : OS_Time) return Integer is
      pragma Unreferenced (Date);
   begin
      return 1;
   end GM_Month;

   function GM_Day    (Date : OS_Time) return Integer is
      pragma Unreferenced (Date);
   begin
      return 1;
   end GM_Day;

   function GM_Hour   (Date : OS_Time) return Integer is
      pragma Unreferenced (Date);
   begin
      return 0;
   end GM_Hour;

   function GM_Minute (Date : OS_Time) return Integer is
      pragma Unreferenced (Date);
   begin
      return 0;
   end GM_Minute;

   function GM_Second (Date : OS_Time) return Integer is
      pragma Unreferenced (Date);
   begin
      return 0;
   end GM_Second;

end GNAT.OS_Lib;
