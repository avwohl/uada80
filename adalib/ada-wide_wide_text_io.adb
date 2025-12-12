-- Ada.Wide_Wide_Text_IO body for Z80
-- Wide_Wide character text I/O implementation

with Ada.Text_IO;

package body Ada.Wide_Wide_Text_IO is

   -- For Z80, Wide_Wide_Text_IO converts to/from Text_IO
   -- using simple character mapping

   type File_Record is record
      Text_File    : Ada.Text_IO.File_Type;
      Mode         : File_Mode := Out_File;
      Is_Open      : Boolean := False;
      Line_Len     : Count := Unbounded;
      Page_Len     : Count := Unbounded;
      Current_Col  : Positive_Count := 1;
      Current_Line : Positive_Count := 1;
      Current_Page : Positive_Count := 1;
   end record;

   -- Standard files
   Std_In  : aliased File_Record;
   Std_Out : aliased File_Record;
   Std_Err : aliased File_Record;

   Cur_In  : File_Access := Std_In'Access;
   Cur_Out : File_Access := Std_Out'Access;
   Cur_Err : File_Access := Std_Err'Access;

   function WWC_To_Char (Item : Wide_Wide_Character) return Character is
      Code : constant Natural := Wide_Wide_Character'Pos (Item);
   begin
      if Code <= 127 then
         return Character'Val (Code);
      else
         return '?';  -- Replacement for non-ASCII
      end if;
   end WWC_To_Char;

   function Char_To_WWC (Item : Character) return Wide_Wide_Character is
   begin
      return Wide_Wide_Character'Val (Character'Pos (Item));
   end Char_To_WWC;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in Out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "")
   is
      pragma Unreferenced (Form);
      TIO_Mode : Ada.Text_IO.File_Mode;
   begin
      File.Ptr := new File_Record;
      case Mode is
         when In_File     => TIO_Mode := Ada.Text_IO.In_File;
         when Out_File    => TIO_Mode := Ada.Text_IO.Out_File;
         when Append_File => TIO_Mode := Ada.Text_IO.Append_File;
      end case;
      Ada.Text_IO.Create (File.Ptr.Text_File, TIO_Mode, Name);
      File.Ptr.Mode := Mode;
      File.Ptr.Is_Open := True;
   end Create;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in Out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "")
   is
      pragma Unreferenced (Form);
      TIO_Mode : Ada.Text_IO.File_Mode;
   begin
      File.Ptr := new File_Record;
      case Mode is
         when In_File     => TIO_Mode := Ada.Text_IO.In_File;
         when Out_File    => TIO_Mode := Ada.Text_IO.Out_File;
         when Append_File => TIO_Mode := Ada.Text_IO.Append_File;
      end case;
      Ada.Text_IO.Open (File.Ptr.Text_File, TIO_Mode, Name);
      File.Ptr.Mode := Mode;
      File.Ptr.Is_Open := True;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (File : in Out File_Type) is
   begin
      if File.Ptr = null or else not File.Ptr.Is_Open then
         raise Status_Error;
      end if;
      Ada.Text_IO.Close (File.Ptr.Text_File);
      File.Ptr.Is_Open := False;
   end Close;

   ------------
   -- Delete --
   ------------

   procedure Delete (File : in Out File_Type) is
   begin
      if File.Ptr = null or else not File.Ptr.Is_Open then
         raise Status_Error;
      end if;
      Ada.Text_IO.Delete (File.Ptr.Text_File);
      File.Ptr.Is_Open := False;
   end Delete;

   -----------
   -- Reset --
   -----------

   procedure Reset (File : in Out File_Type; Mode : File_Mode) is
      TIO_Mode : Ada.Text_IO.File_Mode;
   begin
      if File.Ptr = null or else not File.Ptr.Is_Open then
         raise Status_Error;
      end if;
      case Mode is
         when In_File     => TIO_Mode := Ada.Text_IO.In_File;
         when Out_File    => TIO_Mode := Ada.Text_IO.Out_File;
         when Append_File => TIO_Mode := Ada.Text_IO.Append_File;
      end case;
      Ada.Text_IO.Reset (File.Ptr.Text_File, TIO_Mode);
      File.Ptr.Mode := Mode;
      File.Ptr.Current_Col := 1;
      File.Ptr.Current_Line := 1;
      File.Ptr.Current_Page := 1;
   end Reset;

   procedure Reset (File : in Out File_Type) is
   begin
      Reset (File, File.Ptr.Mode);
   end Reset;

   ----------
   -- Mode --
   ----------

   function Mode (File : File_Type) return File_Mode is
   begin
      if File.Ptr = null or else not File.Ptr.Is_Open then
         raise Status_Error;
      end if;
      return File.Ptr.Mode;
   end Mode;

   ----------
   -- Name --
   ----------

   function Name (File : File_Type) return String is
   begin
      if File.Ptr = null or else not File.Ptr.Is_Open then
         raise Status_Error;
      end if;
      return Ada.Text_IO.Name (File.Ptr.Text_File);
   end Name;

   ----------
   -- Form --
   ----------

   function Form (File : File_Type) return String is
   begin
      if File.Ptr = null or else not File.Ptr.Is_Open then
         raise Status_Error;
      end if;
      return "";
   end Form;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (File : File_Type) return Boolean is
   begin
      return File.Ptr /= null and then File.Ptr.Is_Open;
   end Is_Open;

   ---------------
   -- Set_Input --
   ---------------

   procedure Set_Input (File : File_Type) is
   begin
      Cur_In := File.Ptr;
   end Set_Input;

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output (File : File_Type) is
   begin
      Cur_Out := File.Ptr;
   end Set_Output;

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error (File : File_Type) is
   begin
      Cur_Err := File.Ptr;
   end Set_Error;

   --------------------
   -- Standard_Input --
   --------------------

   function Standard_Input return File_Type is
   begin
      return (Ptr => Std_In'Access);
   end Standard_Input;

   ---------------------
   -- Standard_Output --
   ---------------------

   function Standard_Output return File_Type is
   begin
      return (Ptr => Std_Out'Access);
   end Standard_Output;

   --------------------
   -- Standard_Error --
   --------------------

   function Standard_Error return File_Type is
   begin
      return (Ptr => Std_Err'Access);
   end Standard_Error;

   -------------------
   -- Current_Input --
   -------------------

   function Current_Input return File_Type is
   begin
      return (Ptr => Cur_In);
   end Current_Input;

   --------------------
   -- Current_Output --
   --------------------

   function Current_Output return File_Type is
   begin
      return (Ptr => Cur_Out);
   end Current_Output;

   -------------------
   -- Current_Error --
   -------------------

   function Current_Error return File_Type is
   begin
      return (Ptr => Cur_Err);
   end Current_Error;

   ---------------------
   -- Set_Line_Length --
   ---------------------

   procedure Set_Line_Length (File : File_Type; To : Count) is
   begin
      File.Ptr.Line_Len := To;
   end Set_Line_Length;

   procedure Set_Line_Length (To : Count) is
   begin
      Cur_Out.Line_Len := To;
   end Set_Line_Length;

   ---------------------
   -- Set_Page_Length --
   ---------------------

   procedure Set_Page_Length (File : File_Type; To : Count) is
   begin
      File.Ptr.Page_Len := To;
   end Set_Page_Length;

   procedure Set_Page_Length (To : Count) is
   begin
      Cur_Out.Page_Len := To;
   end Set_Page_Length;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length (File : File_Type) return Count is
   begin
      return File.Ptr.Line_Len;
   end Line_Length;

   function Line_Length return Count is
   begin
      return Cur_Out.Line_Len;
   end Line_Length;

   -----------------
   -- Page_Length --
   -----------------

   function Page_Length (File : File_Type) return Count is
   begin
      return File.Ptr.Page_Len;
   end Page_Length;

   function Page_Length return Count is
   begin
      return Cur_Out.Page_Len;
   end Page_Length;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (File : File_Type; Spacing : Positive_Count := 1) is
   begin
      for I in 1 .. Spacing loop
         Ada.Text_IO.New_Line;
         File.Ptr.Current_Col := 1;
         File.Ptr.Current_Line := File.Ptr.Current_Line + 1;
      end loop;
   end New_Line;

   procedure New_Line (Spacing : Positive_Count := 1) is
   begin
      New_Line (Current_Output, Spacing);
   end New_Line;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line (File : File_Type; Spacing : Positive_Count := 1) is
      C : Character;
   begin
      for I in 1 .. Spacing loop
         while not Ada.Text_IO.End_Of_Line loop
            Ada.Text_IO.Get (C);
         end loop;
         Ada.Text_IO.Skip_Line;
         File.Ptr.Current_Col := 1;
         File.Ptr.Current_Line := File.Ptr.Current_Line + 1;
      end loop;
   end Skip_Line;

   procedure Skip_Line (Spacing : Positive_Count := 1) is
   begin
      Skip_Line (Current_Input, Spacing);
   end Skip_Line;

   -----------------
   -- End_Of_Line --
   -----------------

   function End_Of_Line (File : File_Type) return Boolean is
      pragma Unreferenced (File);
   begin
      return Ada.Text_IO.End_Of_Line;
   end End_Of_Line;

   function End_Of_Line return Boolean is
   begin
      return Ada.Text_IO.End_Of_Line;
   end End_Of_Line;

   --------------
   -- New_Page --
   --------------

   procedure New_Page (File : File_Type) is
   begin
      Ada.Text_IO.New_Page;
      File.Ptr.Current_Col := 1;
      File.Ptr.Current_Line := 1;
      File.Ptr.Current_Page := File.Ptr.Current_Page + 1;
   end New_Page;

   procedure New_Page is
   begin
      New_Page (Current_Output);
   end New_Page;

   ---------------
   -- Skip_Page --
   ---------------

   procedure Skip_Page (File : File_Type) is
   begin
      Ada.Text_IO.Skip_Page;
      File.Ptr.Current_Col := 1;
      File.Ptr.Current_Line := 1;
      File.Ptr.Current_Page := File.Ptr.Current_Page + 1;
   end Skip_Page;

   procedure Skip_Page is
   begin
      Skip_Page (Current_Input);
   end Skip_Page;

   -----------------
   -- End_Of_Page --
   -----------------

   function End_Of_Page (File : File_Type) return Boolean is
      pragma Unreferenced (File);
   begin
      return Ada.Text_IO.End_Of_Page;
   end End_Of_Page;

   function End_Of_Page return Boolean is
   begin
      return Ada.Text_IO.End_Of_Page;
   end End_Of_Page;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : File_Type) return Boolean is
      pragma Unreferenced (File);
   begin
      return Ada.Text_IO.End_Of_File;
   end End_Of_File;

   function End_Of_File return Boolean is
   begin
      return Ada.Text_IO.End_Of_File;
   end End_Of_File;

   -------------
   -- Set_Col --
   -------------

   procedure Set_Col (File : File_Type; To : Positive_Count) is
   begin
      while File.Ptr.Current_Col < To loop
         Put (File, ' ');
      end loop;
   end Set_Col;

   procedure Set_Col (To : Positive_Count) is
   begin
      Set_Col (Current_Output, To);
   end Set_Col;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line (File : File_Type; To : Positive_Count) is
   begin
      while File.Ptr.Current_Line < To loop
         New_Line (File);
      end loop;
   end Set_Line;

   procedure Set_Line (To : Positive_Count) is
   begin
      Set_Line (Current_Output, To);
   end Set_Line;

   ---------
   -- Col --
   ---------

   function Col (File : File_Type) return Positive_Count is
   begin
      return File.Ptr.Current_Col;
   end Col;

   function Col return Positive_Count is
   begin
      return Cur_Out.Current_Col;
   end Col;

   ----------
   -- Line --
   ----------

   function Line (File : File_Type) return Positive_Count is
   begin
      return File.Ptr.Current_Line;
   end Line;

   function Line return Positive_Count is
   begin
      return Cur_Out.Current_Line;
   end Line;

   ----------
   -- Page --
   ----------

   function Page (File : File_Type) return Positive_Count is
   begin
      return File.Ptr.Current_Page;
   end Page;

   function Page return Positive_Count is
   begin
      return Cur_Out.Current_Page;
   end Page;

   ---------
   -- Get --
   ---------

   procedure Get (File : File_Type; Item : out Wide_Wide_Character) is
      C : Character;
   begin
      Ada.Text_IO.Get (C);
      Item := Char_To_WWC (C);
      File.Ptr.Current_Col := File.Ptr.Current_Col + 1;
   end Get;

   procedure Get (Item : out Wide_Wide_Character) is
   begin
      Get (Current_Input, Item);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (File : File_Type; Item : Wide_Wide_Character) is
   begin
      Ada.Text_IO.Put (WWC_To_Char (Item));
      File.Ptr.Current_Col := File.Ptr.Current_Col + 1;
   end Put;

   procedure Put (Item : Wide_Wide_Character) is
   begin
      Put (Current_Output, Item);
   end Put;

   ----------------
   -- Look_Ahead --
   ----------------

   procedure Look_Ahead
     (File        : File_Type;
      Item        : out Wide_Wide_Character;
      End_Of_Line : out Boolean)
   is
      pragma Unreferenced (File);
      C : Character;
   begin
      Ada.Text_IO.Look_Ahead (C, End_Of_Line);
      Item := Char_To_WWC (C);
   end Look_Ahead;

   procedure Look_Ahead
     (Item        : out Wide_Wide_Character;
      End_Of_Line : out Boolean)
   is
   begin
      Look_Ahead (Current_Input, Item, End_Of_Line);
   end Look_Ahead;

   -------------------
   -- Get_Immediate --
   -------------------

   procedure Get_Immediate
     (File : File_Type;
      Item : out Wide_Wide_Character)
   is
      pragma Unreferenced (File);
      C : Character;
   begin
      Ada.Text_IO.Get_Immediate (C);
      Item := Char_To_WWC (C);
   end Get_Immediate;

   procedure Get_Immediate
     (Item : out Wide_Wide_Character)
   is
   begin
      Get_Immediate (Current_Input, Item);
   end Get_Immediate;

   procedure Get_Immediate
     (File      : File_Type;
      Item      : out Wide_Wide_Character;
      Available : out Boolean)
   is
      pragma Unreferenced (File);
      C : Character;
   begin
      Ada.Text_IO.Get_Immediate (C, Available);
      Item := Char_To_WWC (C);
   end Get_Immediate;

   procedure Get_Immediate
     (Item      : out Wide_Wide_Character;
      Available : out Boolean)
   is
   begin
      Get_Immediate (Current_Input, Item, Available);
   end Get_Immediate;

   ---------
   -- Get --
   ---------

   procedure Get (File : File_Type; Item : out Wide_Wide_String) is
      C : Wide_Wide_Character;
   begin
      for I in Item'Range loop
         Get (File, C);
         Item (I) := C;
      end loop;
   end Get;

   procedure Get (Item : out Wide_Wide_String) is
   begin
      Get (Current_Input, Item);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (File : File_Type; Item : Wide_Wide_String) is
   begin
      for I in Item'Range loop
         Put (File, Item (I));
      end loop;
   end Put;

   procedure Put (Item : Wide_Wide_String) is
   begin
      Put (Current_Output, Item);
   end Put;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (File : File_Type;
      Item : out Wide_Wide_String;
      Last : out Natural)
   is
      C : Character;
      S : String (Item'Range);
   begin
      Ada.Text_IO.Get_Line (S, Last);
      for I in Item'First .. Last loop
         Item (I) := Char_To_WWC (S (I));
      end loop;
      File.Ptr.Current_Col := 1;
      File.Ptr.Current_Line := File.Ptr.Current_Line + 1;
   end Get_Line;

   procedure Get_Line
     (Item : out Wide_Wide_String;
      Last : out Natural)
   is
   begin
      Get_Line (Current_Input, Item, Last);
   end Get_Line;

   function Get_Line (File : File_Type) return Wide_Wide_String is
      Buffer : Wide_Wide_String (1 .. Max_Line_Length);
      Last   : Natural;
   begin
      Get_Line (File, Buffer, Last);
      return Buffer (1 .. Last);
   end Get_Line;

   function Get_Line return Wide_Wide_String is
   begin
      return Get_Line (Current_Input);
   end Get_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (File : File_Type; Item : Wide_Wide_String) is
   begin
      Put (File, Item);
      New_Line (File);
   end Put_Line;

   procedure Put_Line (Item : Wide_Wide_String) is
   begin
      Put_Line (Current_Output, Item);
   end Put_Line;

begin
   -- Initialize standard files
   Std_In.Is_Open := True;
   Std_In.Mode := In_File;
   Std_Out.Is_Open := True;
   Std_Out.Mode := Out_File;
   Std_Err.Is_Open := True;
   Std_Err.Mode := Out_File;
end Ada.Wide_Wide_Text_IO;
