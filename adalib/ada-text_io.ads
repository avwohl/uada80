-- Ada.Text_IO specification for Z80
-- Basic text I/O for console operations
--
-- Implementation note: On Z80 systems, this interfaces with CP/M BDOS
-- or direct console I/O depending on target configuration.

package Ada.Text_IO is
   pragma Preelaborate;

   -- File types (simplified for embedded systems)
   type File_Type is limited private;
   type File_Mode is (In_File, Out_File, Append_File);
   type Count is range 0 .. 65535;
   subtype Positive_Count is Count range 1 .. Count'Last;
   subtype Field is Integer range 0 .. 255;
   subtype Number_Base is Integer range 2 .. 16;

   -- Standard input/output/error
   function Standard_Input return File_Type;
   function Standard_Output return File_Type;
   function Standard_Error return File_Type;

   function Current_Input return File_Type;
   function Current_Output return File_Type;
   function Current_Error return File_Type;

   -- File management (limited on embedded systems)
   procedure Create
     (File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "");

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "");

   procedure Close (File : in out File_Type);
   procedure Delete (File : in Out File_Type);
   procedure Reset (File : in Out File_Type; Mode : File_Mode);
   procedure Reset (File : in Out File_Type);

   function Mode (File : File_Type) return File_Mode;
   function Name (File : File_Type) return String;
   function Form (File : File_Type) return String;

   function Is_Open (File : File_Type) return Boolean;

   -- Control operations
   procedure Flush (File : File_Type);
   procedure Flush;  -- Flush Current_Output

   procedure Set_Input (File : File_Type);
   procedure Set_Output (File : File_Type);
   procedure Set_Error (File : File_Type);

   -- Line and page control
   procedure New_Line (File : File_Type; Spacing : Positive_Count := 1);
   procedure New_Line (Spacing : Positive_Count := 1);

   procedure Skip_Line (File : File_Type; Spacing : Positive_Count := 1);
   procedure Skip_Line (Spacing : Positive_Count := 1);

   function End_Of_Line (File : File_Type) return Boolean;
   function End_Of_Line return Boolean;

   procedure New_Page (File : File_Type);
   procedure New_Page;

   procedure Skip_Page (File : File_Type);
   procedure Skip_Page;

   function End_Of_Page (File : File_Type) return Boolean;
   function End_Of_Page return Boolean;

   function End_Of_File (File : File_Type) return Boolean;
   function End_Of_File return Boolean;

   procedure Set_Line_Length (File : File_Type; To : Count);
   procedure Set_Line_Length (To : Count);

   procedure Set_Page_Length (File : File_Type; To : Count);
   procedure Set_Page_Length (To : Count);

   function Line_Length (File : File_Type) return Count;
   function Line_Length return Count;

   function Page_Length (File : File_Type) return Count;
   function Page_Length return Count;

   function Line (File : File_Type) return Positive_Count;
   function Line return Positive_Count;

   function Page (File : File_Type) return Positive_Count;
   function Page return Positive_Count;

   function Col (File : File_Type) return Positive_Count;
   function Col return Positive_Count;

   procedure Set_Col (File : File_Type; To : Positive_Count);
   procedure Set_Col (To : Positive_Count);

   procedure Set_Line (File : File_Type; To : Positive_Count);
   procedure Set_Line (To : Positive_Count);

   -- Character I/O
   procedure Get (File : File_Type; Item : out Character);
   procedure Get (Item : out Character);

   procedure Put (File : File_Type; Item : Character);
   procedure Put (Item : Character);

   procedure Look_Ahead
     (File        : File_Type;
      Item        : out Character;
      End_Of_Line : out Boolean);

   procedure Look_Ahead
     (Item        : out Character;
      End_Of_Line : out Boolean);

   procedure Get_Immediate
     (File : File_Type;
      Item : out Character);

   procedure Get_Immediate (Item : out Character);

   procedure Get_Immediate
     (File      : File_Type;
      Item      : out Character;
      Available : out Boolean);

   procedure Get_Immediate
     (Item      : out Character;
      Available : out Boolean);

   -- String I/O
   procedure Get (File : File_Type; Item : out String);
   procedure Get (Item : out String);

   procedure Put (File : File_Type; Item : String);
   procedure Put (Item : String);

   procedure Get_Line
     (File : File_Type;
      Item : out String;
      Last : out Natural);

   procedure Get_Line
     (Item : out String;
      Last : out Natural);

   function Get_Line (File : File_Type) return String;
   function Get_Line return String;

   procedure Put_Line (File : File_Type; Item : String);
   procedure Put_Line (Item : String);

   -- Exceptions
   Status_Error : exception;
   Mode_Error   : exception;
   Name_Error   : exception;
   Use_Error    : exception;
   Device_Error : exception;
   End_Error    : exception;
   Data_Error   : exception;
   Layout_Error : exception;

private
   -- File type implementation (simplified for Z80)
   type File_Handle is range 0 .. 255;
   Closed_Handle : constant File_Handle := 255;

   type File_Type is record
      Handle    : File_Handle := Closed_Handle;
      Mode      : File_Mode := In_File;
      Is_Open   : Boolean := False;
      Col       : Positive_Count := 1;
      Line      : Positive_Count := 1;
      Page      : Positive_Count := 1;
      Line_Len  : Count := 0;  -- 0 = unbounded
      Page_Len  : Count := 0;  -- 0 = unbounded
   end record;

end Ada.Text_IO;
