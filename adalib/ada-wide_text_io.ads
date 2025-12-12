-- Ada.Wide_Text_IO for Z80
-- Wide character text I/O operations
--
-- Note: On Z80 with limited memory and typically ASCII-only displays,
-- Wide_Text_IO maps wide characters to their ASCII equivalents when possible,
-- or uses a placeholder for non-representable characters.

package Ada.Wide_Text_IO is
   pragma Preelaborate;

   -- File types - reuse standard file handle approach
   type File_Type is limited private;

   type File_Mode is (In_File, Out_File, Append_File);

   type Count is range 0 .. 65535;
   subtype Positive_Count is Count range 1 .. Count'Last;

   Unbounded : constant Count := 0;

   subtype Field is Integer range 0 .. 255;
   subtype Number_Base is Integer range 2 .. 16;

   -- Standard files
   function Standard_Input return File_Type;
   function Standard_Output return File_Type;
   function Standard_Error return File_Type;

   function Current_Input return File_Type;
   function Current_Output return File_Type;
   function Current_Error return File_Type;

   -- File management
   procedure Create
     (File : in Out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "");

   procedure Open
     (File : in Out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "");

   procedure Close (File : in Out File_Type);
   procedure Delete (File : in Out File_Type);
   procedure Reset (File : in Out File_Type; Mode : File_Mode);
   procedure Reset (File : in Out File_Type);

   function Mode (File : File_Type) return File_Mode;
   function Name (File : File_Type) return String;
   function Form (File : File_Type) return String;

   function Is_Open (File : File_Type) return Boolean;

   -- Wide character I/O
   procedure Get (File : File_Type; Item : out Wide_Character);
   procedure Get (Item : out Wide_Character);

   procedure Put (File : File_Type; Item : Wide_Character);
   procedure Put (Item : Wide_Character);

   procedure Look_Ahead
     (File        : File_Type;
      Item        : out Wide_Character;
      End_Of_Line : out Boolean);

   procedure Look_Ahead
     (Item        : out Wide_Character;
      End_Of_Line : out Boolean);

   procedure Get_Immediate
     (File : File_Type;
      Item : out Wide_Character);

   procedure Get_Immediate
     (Item : out Wide_Character);

   procedure Get_Immediate
     (File      : File_Type;
      Item      : out Wide_Character;
      Available : out Boolean);

   procedure Get_Immediate
     (Item      : out Wide_Character;
      Available : out Boolean);

   -- Wide string I/O
   procedure Get (File : File_Type; Item : out Wide_String);
   procedure Get (Item : out Wide_String);

   procedure Put (File : File_Type; Item : Wide_String);
   procedure Put (Item : Wide_String);

   procedure Get_Line
     (File : File_Type;
      Item : out Wide_String;
      Last : out Natural);

   procedure Get_Line
     (Item : out Wide_String;
      Last : out Natural);

   procedure Put_Line (File : File_Type; Item : Wide_String);
   procedure Put_Line (Item : Wide_String);

   -- Line/page operations
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

   -- Column/line/page operations
   procedure Set_Col (File : File_Type; To : Positive_Count);
   procedure Set_Col (To : Positive_Count);

   procedure Set_Line (File : File_Type; To : Positive_Count);
   procedure Set_Line (To : Positive_Count);

   function Col (File : File_Type) return Positive_Count;
   function Col return Positive_Count;

   function Line (File : File_Type) return Positive_Count;
   function Line return Positive_Count;

   function Page (File : File_Type) return Positive_Count;
   function Page return Positive_Count;

   -- Page length operations
   procedure Set_Line_Length (File : File_Type; To : Count);
   procedure Set_Line_Length (To : Count);

   procedure Set_Page_Length (File : File_Type; To : Count);
   procedure Set_Page_Length (To : Count);

   function Line_Length (File : File_Type) return Count;
   function Line_Length return Count;

   function Page_Length (File : File_Type) return Count;
   function Page_Length return Count;

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

   type File_Type is record
      Handle      : Integer := -1;
      Mode        : File_Mode := In_File;
      Is_Open     : Boolean := False;
      Is_Standard : Boolean := False;
      Col         : Positive_Count := 1;
      Line        : Positive_Count := 1;
      Page        : Positive_Count := 1;
      Line_Length : Count := Unbounded;
      Page_Length : Count := Unbounded;
   end record;

end Ada.Wide_Text_IO;
