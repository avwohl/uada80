-- Ada.Wide_Wide_Text_IO for Z80
-- Wide_Wide character text I/O

with Ada.IO_Exceptions;

package Ada.Wide_Wide_Text_IO is

   type File_Type is limited private;

   type File_Mode is (In_File, Out_File, Append_File);

   type Count is range 0 .. 2**15 - 1;
   subtype Positive_Count is Count range 1 .. Count'Last;
   Unbounded : constant Count := 0;

   subtype Field is Integer range 0 .. 255;
   subtype Number_Base is Integer range 2 .. 16;

   type Type_Set is (Lower_Case, Upper_Case);

   -- File Management

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

   -- Control of default input, output, and error files

   procedure Set_Input (File : File_Type);
   procedure Set_Output (File : File_Type);
   procedure Set_Error (File : File_Type);

   function Standard_Input return File_Type;
   function Standard_Output return File_Type;
   function Standard_Error return File_Type;

   function Current_Input return File_Type;
   function Current_Output return File_Type;
   function Current_Error return File_Type;

   -- Specification of line and page lengths

   procedure Set_Line_Length (File : File_Type; To : Count);
   procedure Set_Line_Length (To : Count);

   procedure Set_Page_Length (File : File_Type; To : Count);
   procedure Set_Page_Length (To : Count);

   function Line_Length (File : File_Type) return Count;
   function Line_Length return Count;

   function Page_Length (File : File_Type) return Count;
   function Page_Length return Count;

   -- Column, Line, and Page Control

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

   -- Wide_Wide_Character Input-Output

   procedure Get (File : File_Type; Item : out Wide_Wide_Character);
   procedure Get (Item : out Wide_Wide_Character);

   procedure Put (File : File_Type; Item : Wide_Wide_Character);
   procedure Put (Item : Wide_Wide_Character);

   procedure Look_Ahead
     (File        : File_Type;
      Item        : out Wide_Wide_Character;
      End_Of_Line : out Boolean);

   procedure Look_Ahead
     (Item        : out Wide_Wide_Character;
      End_Of_Line : out Boolean);

   procedure Get_Immediate
     (File : File_Type;
      Item : out Wide_Wide_Character);

   procedure Get_Immediate
     (Item : out Wide_Wide_Character);

   procedure Get_Immediate
     (File      : File_Type;
      Item      : out Wide_Wide_Character;
      Available : out Boolean);

   procedure Get_Immediate
     (Item      : out Wide_Wide_Character;
      Available : out Boolean);

   -- Wide_Wide_String Input-Output

   procedure Get (File : File_Type; Item : out Wide_Wide_String);
   procedure Get (Item : out Wide_Wide_String);

   procedure Put (File : File_Type; Item : Wide_Wide_String);
   procedure Put (Item : Wide_Wide_String);

   procedure Get_Line
     (File : File_Type;
      Item : out Wide_Wide_String;
      Last : out Natural);

   procedure Get_Line
     (Item : out Wide_Wide_String;
      Last : out Natural);

   function Get_Line (File : File_Type) return Wide_Wide_String;
   function Get_Line return Wide_Wide_String;

   procedure Put_Line (File : File_Type; Item : Wide_Wide_String);
   procedure Put_Line (Item : Wide_Wide_String);

   -- Exceptions

   Status_Error : exception renames Ada.IO_Exceptions.Status_Error;
   Mode_Error   : exception renames Ada.IO_Exceptions.Mode_Error;
   Name_Error   : exception renames Ada.IO_Exceptions.Name_Error;
   Use_Error    : exception renames Ada.IO_Exceptions.Use_Error;
   Device_Error : exception renames Ada.IO_Exceptions.Device_Error;
   End_Error    : exception renames Ada.IO_Exceptions.End_Error;
   Data_Error   : exception renames Ada.IO_Exceptions.Data_Error;
   Layout_Error : exception renames Ada.IO_Exceptions.Layout_Error;

private

   -- Maximum line length for Z80
   Max_Line_Length : constant := 255;

   type File_Record;
   type File_Access is access File_Record;

   type File_Type is record
      Ptr : File_Access := null;
   end record;

end Ada.Wide_Wide_Text_IO;
