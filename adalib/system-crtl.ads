-- System.CRTL for Z80
-- C runtime library interface (CP/M BDOS calls)

package System.CRTL is
   pragma Preelaborate;

   -- CP/M BDOS function codes
   System_Reset     : constant := 0;
   Console_Input    : constant := 1;
   Console_Output   : constant := 2;
   Reader_Input     : constant := 3;
   Punch_Output     : constant := 4;
   List_Output      : constant := 5;
   Direct_Console   : constant := 6;
   Get_IO_Byte      : constant := 7;
   Set_IO_Byte      : constant := 8;
   Print_String     : constant := 9;
   Read_Console     : constant := 10;
   Console_Status   : constant := 11;
   Get_Version      : constant := 12;
   Reset_Disk       : constant := 13;
   Select_Disk      : constant := 14;
   Open_File        : constant := 15;
   Close_File       : constant := 16;
   Search_First     : constant := 17;
   Search_Next      : constant := 18;
   Delete_File      : constant := 19;
   Read_Sequential  : constant := 20;
   Write_Sequential : constant := 21;
   Make_File        : constant := 22;
   Rename_File_F    : constant := 23;
   Get_Login_Vector : constant := 24;
   Get_Current_Disk : constant := 25;
   Set_DMA          : constant := 26;
   Get_Alloc_Vector : constant := 27;
   Write_Protect    : constant := 28;
   Get_RO_Vector    : constant := 29;
   Set_File_Attr    : constant := 30;
   Get_Disk_Params  : constant := 31;
   Get_Set_User     : constant := 32;
   Read_Random      : constant := 33;
   Write_Random     : constant := 34;
   Compute_Size     : constant := 35;
   Set_Random_Rec   : constant := 36;

   -- BDOS call
   function BDOS (Func : Natural; DE : Natural := 0) return Natural;
   pragma Import (Assembler, BDOS, "_bdos");

   -- File operations (high-level wrappers)
   function Open_Read (Name : String) return Integer;
   function Open_Read_Write (Name : String) return Integer;
   function Create (Name : String) return Integer;
   procedure Close (FD : Integer);
   function Read (FD : Integer; Buf : System.Address; Len : Integer) return Integer;
   function Write (FD : Integer; Buf : System.Address; Len : Integer) return Integer;
   function Unlink (Name : String) return Integer;
   function Rename (Old_Name, New_Name : String) return Integer;

   -- Console I/O
   function Getchar return Integer;
   procedure Putchar (C : Integer);
   function Kbhit return Boolean;

   -- Program control
   procedure Exit_Program (Status : Integer);
   pragma No_Return (Exit_Program);

   -- Memory operations
   procedure Memcpy (Dest, Src : System.Address; N : Natural);
   procedure Memset (Dest : System.Address; C : Integer; N : Natural);
   function Memcmp (S1, S2 : System.Address; N : Natural) return Integer;

   -- String operations
   function Strlen (S : System.Address) return Natural;

end System.CRTL;
