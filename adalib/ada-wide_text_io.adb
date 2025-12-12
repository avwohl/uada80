-- Ada.Wide_Text_IO body for Z80
-- Wide character text I/O implementation
--
-- Maps Wide_Character to ASCII where possible

package body Ada.Wide_Text_IO is

   -- Standard file handles
   Std_In  : File_Type := (Handle => 0, Mode => In_File, Is_Open => True, Is_Standard => True, others => <>);
   Std_Out : File_Type := (Handle => 1, Mode => Out_File, Is_Open => True, Is_Standard => True, others => <>);
   Std_Err : File_Type := (Handle => 2, Mode => Out_File, Is_Open => True, Is_Standard => True, others => <>);

   Cur_In  : File_Type := Std_In;
   Cur_Out : File_Type := Std_Out;
   Cur_Err : File_Type := Std_Err;

   -- Convert Wide_Character to Character (lossy for non-ASCII)
   function To_Char (WC : Wide_Character) return Character is
      Pos : Natural := Wide_Character'Pos (WC);
   begin
      if Pos <= 127 then
         return Character'Val (Pos);
      else
         return '?';  -- Placeholder for non-ASCII
      end if;
   end To_Char;

   -- Convert Character to Wide_Character
   function To_Wide (C : Character) return Wide_Character is
   begin
      return Wide_Character'Val (Character'Pos (C));
   end To_Wide;

   -- CP/M BDOS calls
   procedure BDOS_ConOut (C : Character);
   pragma Import (Assembler, BDOS_ConOut, "_conout");

   function BDOS_ConIn return Character;
   pragma Import (Assembler, BDOS_ConIn, "_conin");

   function BDOS_ConStat return Boolean;
   pragma Import (Assembler, BDOS_ConStat, "_constat");

   --------------------
   -- Standard_Input --
   --------------------

   function Standard_Input return File_Type is
   begin
      return Std_In;
   end Standard_Input;

   ---------------------
   -- Standard_Output --
   ---------------------

   function Standard_Output return File_Type is
   begin
      return Std_Out;
   end Standard_Output;

   --------------------
   -- Standard_Error --
   --------------------

   function Standard_Error return File_Type is
   begin
      return Std_Err;
   end Standard_Error;

   -------------------
   -- Current_Input --
   -------------------

   function Current_Input return File_Type is
   begin
      return Cur_In;
   end Current_Input;

   --------------------
   -- Current_Output --
   --------------------

   function Current_Output return File_Type is
   begin
      return Cur_Out;
   end Current_Output;

   -------------------
   -- Current_Error --
   -------------------

   function Current_Error return File_Type is
   begin
      return Cur_Err;
   end Current_Error;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in Out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "")
   is
      pragma Unreferenced (Name);
      pragma Unreferenced (Form);
   begin
      File.Mode := Mode;
      File.Is_Open := True;
      File.Col := 1;
      File.Line := 1;
      File.Page := 1;
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
   begin
      Create (File, Mode, Name, Form);
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (File : in Out File_Type) is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;
      File.Is_Open := False;
   end Close;

   ------------
   -- Delete --
   ------------

   procedure Delete (File : in Out File_Type) is
   begin
      Close (File);
   end Delete;

   -----------
   -- Reset --
   -----------

   procedure Reset (File : in Out File_Type; Mode : File_Mode) is
   begin
      File.Mode := Mode;
      File.Col := 1;
      File.Line := 1;
      File.Page := 1;
   end Reset;

   procedure Reset (File : in Out File_Type) is
   begin
      Reset (File, File.Mode);
   end Reset;

   ----------
   -- Mode --
   ----------

   function Mode (File : File_Type) return File_Mode is
   begin
      return File.Mode;
   end Mode;

   ----------
   -- Name --
   ----------

   function Name (File : File_Type) return String is
      pragma Unreferenced (File);
   begin
      return "";
   end Name;

   ----------
   -- Form --
   ----------

   function Form (File : File_Type) return String is
      pragma Unreferenced (File);
   begin
      return "";
   end Form;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (File : File_Type) return Boolean is
   begin
      return File.Is_Open;
   end Is_Open;

   ---------
   -- Get --
   ---------

   procedure Get (File : File_Type; Item : out Wide_Character) is
      C : Character;
   begin
      if File.Mode /= In_File then
         raise Mode_Error;
      end if;
      C := BDOS_ConIn;
      Item := To_Wide (C);
   end Get;

   procedure Get (Item : out Wide_Character) is
   begin
      Get (Cur_In, Item);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (File : File_Type; Item : Wide_Character) is
   begin
      if File.Mode = In_File then
         raise Mode_Error;
      end if;
      BDOS_ConOut (To_Char (Item));
   end Put;

   procedure Put (Item : Wide_Character) is
   begin
      Put (Cur_Out, Item);
   end Put;

   ----------------
   -- Look_Ahead --
   ----------------

   procedure Look_Ahead
     (File        : File_Type;
      Item        : out Wide_Character;
      End_Of_Line : out Boolean)
   is
      pragma Unreferenced (File);
   begin
      Item := Wide_Character'Val (0);
      End_Of_Line := True;
   end Look_Ahead;

   procedure Look_Ahead
     (Item        : out Wide_Character;
      End_Of_Line : out Boolean)
   is
   begin
      Look_Ahead (Cur_In, Item, End_Of_Line);
   end Look_Ahead;

   -------------------
   -- Get_Immediate --
   -------------------

   procedure Get_Immediate
     (File : File_Type;
      Item : out Wide_Character)
   is
   begin
      Get (File, Item);
   end Get_Immediate;

   procedure Get_Immediate
     (Item : out Wide_Character)
   is
   begin
      Get (Item);
   end Get_Immediate;

   procedure Get_Immediate
     (File      : File_Type;
      Item      : out Wide_Character;
      Available : out Boolean)
   is
      pragma Unreferenced (File);
   begin
      Available := BDOS_ConStat;
      if Available then
         Item := To_Wide (BDOS_ConIn);
      else
         Item := Wide_Character'Val (0);
      end if;
   end Get_Immediate;

   procedure Get_Immediate
     (Item      : out Wide_Character;
      Available : out Boolean)
   is
   begin
      Get_Immediate (Cur_In, Item, Available);
   end Get_Immediate;

   -- Wide_String Get
   procedure Get (File : File_Type; Item : out Wide_String) is
   begin
      for I in Item'Range loop
         Get (File, Item (I));
      end loop;
   end Get;

   procedure Get (Item : out Wide_String) is
   begin
      Get (Cur_In, Item);
   end Get;

   -- Wide_String Put
   procedure Put (File : File_Type; Item : Wide_String) is
   begin
      for I in Item'Range loop
         Put (File, Item (I));
      end loop;
   end Put;

   procedure Put (Item : Wide_String) is
   begin
      Put (Cur_Out, Item);
   end Put;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (File : File_Type;
      Item : out Wide_String;
      Last : out Natural)
   is
      C : Wide_Character;
   begin
      Last := Item'First - 1;
      for I in Item'Range loop
         Get (File, C);
         exit when Wide_Character'Pos (C) = 13 or Wide_Character'Pos (C) = 10;
         Item (I) := C;
         Last := I;
      end loop;
   end Get_Line;

   procedure Get_Line
     (Item : out Wide_String;
      Last : out Natural)
   is
   begin
      Get_Line (Cur_In, Item, Last);
   end Get_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (File : File_Type; Item : Wide_String) is
   begin
      Put (File, Item);
      New_Line (File);
   end Put_Line;

   procedure Put_Line (Item : Wide_String) is
   begin
      Put_Line (Cur_Out, Item);
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (File : File_Type; Spacing : Positive_Count := 1) is
   begin
      for I in 1 .. Spacing loop
         BDOS_ConOut (Character'Val (13));
         BDOS_ConOut (Character'Val (10));
      end loop;
   end New_Line;

   procedure New_Line (Spacing : Positive_Count := 1) is
   begin
      New_Line (Cur_Out, Spacing);
   end New_Line;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line (File : File_Type; Spacing : Positive_Count := 1) is
      C : Wide_Character;
   begin
      for I in 1 .. Spacing loop
         loop
            Get (File, C);
            exit when Wide_Character'Pos (C) = 10;
         end loop;
      end loop;
   end Skip_Line;

   procedure Skip_Line (Spacing : Positive_Count := 1) is
   begin
      Skip_Line (Cur_In, Spacing);
   end Skip_Line;

   -----------------
   -- End_Of_Line --
   -----------------

   function End_Of_Line (File : File_Type) return Boolean is
      pragma Unreferenced (File);
   begin
      return False;  -- Simplified
   end End_Of_Line;

   function End_Of_Line return Boolean is
   begin
      return End_Of_Line (Cur_In);
   end End_Of_Line;

   --------------
   -- New_Page --
   --------------

   procedure New_Page (File : File_Type) is
   begin
      BDOS_ConOut (Character'Val (12));  -- Form feed
   end New_Page;

   procedure New_Page is
   begin
      New_Page (Cur_Out);
   end New_Page;

   ---------------
   -- Skip_Page --
   ---------------

   procedure Skip_Page (File : File_Type) is
      C : Wide_Character;
   begin
      loop
         Get (File, C);
         exit when Wide_Character'Pos (C) = 12;
      end loop;
   end Skip_Page;

   procedure Skip_Page is
   begin
      Skip_Page (Cur_In);
   end Skip_Page;

   -----------------
   -- End_Of_Page --
   -----------------

   function End_Of_Page (File : File_Type) return Boolean is
      pragma Unreferenced (File);
   begin
      return False;
   end End_Of_Page;

   function End_Of_Page return Boolean is
   begin
      return End_Of_Page (Cur_In);
   end End_Of_Page;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : File_Type) return Boolean is
      pragma Unreferenced (File);
   begin
      return False;
   end End_Of_File;

   function End_Of_File return Boolean is
   begin
      return End_Of_File (Cur_In);
   end End_Of_File;

   -------------
   -- Set_Col --
   -------------

   procedure Set_Col (File : File_Type; To : Positive_Count) is
      pragma Unreferenced (File);
      pragma Unreferenced (To);
   begin
      null;  -- Not implemented for console
   end Set_Col;

   procedure Set_Col (To : Positive_Count) is
   begin
      Set_Col (Cur_Out, To);
   end Set_Col;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line (File : File_Type; To : Positive_Count) is
      pragma Unreferenced (File);
      pragma Unreferenced (To);
   begin
      null;
   end Set_Line;

   procedure Set_Line (To : Positive_Count) is
   begin
      Set_Line (Cur_Out, To);
   end Set_Line;

   ---------
   -- Col --
   ---------

   function Col (File : File_Type) return Positive_Count is
   begin
      return File.Col;
   end Col;

   function Col return Positive_Count is
   begin
      return Col (Cur_Out);
   end Col;

   ----------
   -- Line --
   ----------

   function Line (File : File_Type) return Positive_Count is
   begin
      return File.Line;
   end Line;

   function Line return Positive_Count is
   begin
      return Line (Cur_Out);
   end Line;

   ----------
   -- Page --
   ----------

   function Page (File : File_Type) return Positive_Count is
   begin
      return File.Page;
   end Page;

   function Page return Positive_Count is
   begin
      return Page (Cur_Out);
   end Page;

   ---------------------
   -- Set_Line_Length --
   ---------------------

   procedure Set_Line_Length (File : File_Type; To : Count) is
      pragma Unreferenced (File);
      pragma Unreferenced (To);
   begin
      null;
   end Set_Line_Length;

   procedure Set_Line_Length (To : Count) is
   begin
      Set_Line_Length (Cur_Out, To);
   end Set_Line_Length;

   ---------------------
   -- Set_Page_Length --
   ---------------------

   procedure Set_Page_Length (File : File_Type; To : Count) is
      pragma Unreferenced (File);
      pragma Unreferenced (To);
   begin
      null;
   end Set_Page_Length;

   procedure Set_Page_Length (To : Count) is
   begin
      Set_Page_Length (Cur_Out, To);
   end Set_Page_Length;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length (File : File_Type) return Count is
   begin
      return File.Line_Length;
   end Line_Length;

   function Line_Length return Count is
   begin
      return Line_Length (Cur_Out);
   end Line_Length;

   -----------------
   -- Page_Length --
   -----------------

   function Page_Length (File : File_Type) return Count is
   begin
      return File.Page_Length;
   end Page_Length;

   function Page_Length return Count is
   begin
      return Page_Length (Cur_Out);
   end Page_Length;

end Ada.Wide_Text_IO;
