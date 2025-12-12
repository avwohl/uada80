-- Ada.Text_IO implementation for Z80
-- Provides basic text I/O through runtime library calls
--
-- Z80 runtime interface:
--   _putchar(c)   - Output character
--   _getchar()    - Input character (blocking)
--   _kbhit()      - Check if character available
--   _puts(str)    - Output string
--   _gets(buf,n)  - Input line

package body Ada.Text_IO is

   -- Runtime library imports (implemented in Z80 assembly)
   procedure Rt_Putchar (C : Character);
   pragma Import (C, Rt_Putchar, "_putchar");

   function Rt_Getchar return Character;
   pragma Import (C, Rt_Getchar, "_getchar");

   function Rt_Kbhit return Integer;
   pragma Import (C, Rt_Kbhit, "_kbhit");

   procedure Rt_Puts (S : String);
   pragma Import (C, Rt_Puts, "_puts");

   -- Standard file handles (pre-opened)
   Stdin  : aliased File_Type :=
     (Handle => 0, Mode => In_File, Is_Open => True, others => <>);
   Stdout : aliased File_Type :=
     (Handle => 1, Mode => Out_File, Is_Open => True, others => <>);
   Stderr : aliased File_Type :=
     (Handle => 2, Mode => Out_File, Is_Open => True, others => <>);

   Current_In  : access File_Type := Stdin'Access;
   Current_Out : access File_Type := Stdout'Access;
   Current_Err : access File_Type := Stderr'Access;

   -- Standard input/output functions
   function Standard_Input return File_Type is
   begin
      return Stdin;
   end Standard_Input;

   function Standard_Output return File_Type is
   begin
      return Stdout;
   end Standard_Output;

   function Standard_Error return File_Type is
   begin
      return Stderr;
   end Standard_Error;

   function Current_Input return File_Type is
   begin
      return Current_In.all;
   end Current_Input;

   function Current_Output return File_Type is
   begin
      return Current_Out.all;
   end Current_Output;

   function Current_Error return File_Type is
   begin
      return Current_Err.all;
   end Current_Error;

   -- File management (minimal implementation for embedded)
   procedure Create
     (File : in Out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "")
   is
   begin
      -- For console-only systems, create always fails
      -- unless Name is empty (creates a temp console stream)
      if Name = "" then
         File.Handle := 1;  -- Console
         File.Mode := Mode;
         File.Is_Open := True;
         File.Col := 1;
         File.Line := 1;
         File.Page := 1;
      else
         raise Name_Error;
      end if;
   end Create;

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "")
   is
   begin
      -- On minimal Z80 systems, only console I/O is available
      raise Name_Error;
   end Open;

   procedure Close (File : in Out File_Type) is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;
      File.Is_Open := False;
      File.Handle := Closed_Handle;
   end Close;

   procedure Delete (File : in out File_Type) is
   begin
      raise Use_Error;  -- Not supported on embedded
   end Delete;

   procedure Reset (File : in Out File_Type; Mode : File_Mode) is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;
      File.Mode := Mode;
      File.Col := 1;
      File.Line := 1;
      File.Page := 1;
   end Reset;

   procedure Reset (File : in Out File_Type) is
   begin
      Reset (File, File.Mode);
   end Reset;

   function Mode (File : File_Type) return File_Mode is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;
      return File.Mode;
   end Mode;

   function Name (File : File_Type) return String is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;
      return "CONSOLE";
   end Name;

   function Form (File : File_Type) return String is
   begin
      return "";
   end Form;

   function Is_Open (File : File_Type) return Boolean is
   begin
      return File.Is_Open;
   end Is_Open;

   -- Flush operations
   procedure Flush (File : File_Type) is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;
      -- Console output is typically unbuffered on Z80
      null;
   end Flush;

   procedure Flush is
   begin
      Flush (Current_Out.all);
   end Flush;

   procedure Set_Input (File : File_Type) is
   begin
      if not File.Is_Open or File.Mode /= In_File then
         raise Mode_Error;
      end if;
      -- For simplicity, ignore (only console supported)
   end Set_Input;

   procedure Set_Output (File : File_Type) is
   begin
      if not File.Is_Open or File.Mode = In_File then
         raise Mode_Error;
      end if;
      null;
   end Set_Output;

   procedure Set_Error (File : File_Type) is
   begin
      if not File.Is_Open or File.Mode = In_File then
         raise Mode_Error;
      end if;
      null;
   end Set_Error;

   -- Line/page control
   procedure New_Line (File : File_Type; Spacing : Positive_Count := 1) is
   begin
      if not File.Is_Open or File.Mode = In_File then
         raise Mode_Error;
      end if;
      for I in 1 .. Spacing loop
         Rt_Putchar (Character'Val (13));  -- CR
         Rt_Putchar (Character'Val (10));  -- LF
      end loop;
   end New_Line;

   procedure New_Line (Spacing : Positive_Count := 1) is
   begin
      New_Line (Current_Out.all, Spacing);
   end New_Line;

   procedure Skip_Line (File : File_Type; Spacing : Positive_Count := 1) is
      C : Character;
   begin
      if not File.Is_Open or File.Mode /= In_File then
         raise Mode_Error;
      end if;
      for I in 1 .. Spacing loop
         loop
            C := Rt_Getchar;
            exit when C = Character'Val (10);  -- LF
         end loop;
      end loop;
   end Skip_Line;

   procedure Skip_Line (Spacing : Positive_Count := 1) is
   begin
      Skip_Line (Current_In.all, Spacing);
   end Skip_Line;

   function End_Of_Line (File : File_Type) return Boolean is
   begin
      -- Would need look-ahead; for now always false
      return False;
   end End_Of_Line;

   function End_Of_Line return Boolean is
   begin
      return End_Of_Line (Current_In.all);
   end End_Of_Line;

   procedure New_Page (File : File_Type) is
   begin
      if not File.Is_Open or File.Mode = In_File then
         raise Mode_Error;
      end if;
      Rt_Putchar (Character'Val (12));  -- Form feed
   end New_Page;

   procedure New_Page is
   begin
      New_Page (Current_Out.all);
   end New_Page;

   procedure Skip_Page (File : File_Type) is
      C : Character;
   begin
      if not File.Is_Open or File.Mode /= In_File then
         raise Mode_Error;
      end if;
      loop
         C := Rt_Getchar;
         exit when C = Character'Val (12);  -- Form feed
      end loop;
   end Skip_Page;

   procedure Skip_Page is
   begin
      Skip_Page (Current_In.all);
   end Skip_Page;

   function End_Of_Page (File : File_Type) return Boolean is
   begin
      return False;  -- Simplified
   end End_Of_Page;

   function End_Of_Page return Boolean is
   begin
      return End_Of_Page (Current_In.all);
   end End_Of_Page;

   function End_Of_File (File : File_Type) return Boolean is
   begin
      return False;  -- Console never ends
   end End_Of_File;

   function End_Of_File return Boolean is
   begin
      return End_Of_File (Current_In.all);
   end End_Of_File;

   -- Line/page length control
   procedure Set_Line_Length (File : File_Type; To : Count) is
   begin
      null;  -- Ignore on console
   end Set_Line_Length;

   procedure Set_Line_Length (To : Count) is
   begin
      Set_Line_Length (Current_Out.all, To);
   end Set_Line_Length;

   procedure Set_Page_Length (File : File_Type; To : Count) is
   begin
      null;  -- Ignore on console
   end Set_Page_Length;

   procedure Set_Page_Length (To : Count) is
   begin
      Set_Page_Length (Current_Out.all, To);
   end Set_Page_Length;

   function Line_Length (File : File_Type) return Count is
   begin
      return 0;  -- Unbounded
   end Line_Length;

   function Line_Length return Count is
   begin
      return Line_Length (Current_Out.all);
   end Line_Length;

   function Page_Length (File : File_Type) return Count is
   begin
      return 0;  -- Unbounded
   end Page_Length;

   function Page_Length return Count is
   begin
      return Page_Length (Current_Out.all);
   end Page_Length;

   function Line (File : File_Type) return Positive_Count is
   begin
      return File.Line;
   end Line;

   function Line return Positive_Count is
   begin
      return Line (Current_Out.all);
   end Line;

   function Page (File : File_Type) return Positive_Count is
   begin
      return File.Page;
   end Page;

   function Page return Positive_Count is
   begin
      return Page (Current_Out.all);
   end Page;

   function Col (File : File_Type) return Positive_Count is
   begin
      return File.Col;
   end Col;

   function Col return Positive_Count is
   begin
      return Col (Current_Out.all);
   end Col;

   procedure Set_Col (File : File_Type; To : Positive_Count) is
   begin
      while File.Col < To loop
         Rt_Putchar (' ');
      end loop;
   end Set_Col;

   procedure Set_Col (To : Positive_Count) is
   begin
      Set_Col (Current_Out.all, To);
   end Set_Col;

   procedure Set_Line (File : File_Type; To : Positive_Count) is
   begin
      while File.Line < To loop
         New_Line (File);
      end loop;
   end Set_Line;

   procedure Set_Line (To : Positive_Count) is
   begin
      Set_Line (Current_Out.all, To);
   end Set_Line;

   -- Character I/O
   procedure Get (File : File_Type; Item : out Character) is
   begin
      if not File.Is_Open or File.Mode /= In_File then
         raise Mode_Error;
      end if;
      Item := Rt_Getchar;
   end Get;

   procedure Get (Item : out Character) is
   begin
      Get (Current_In.all, Item);
   end Get;

   procedure Put (File : File_Type; Item : Character) is
   begin
      if not File.Is_Open or File.Mode = In_File then
         raise Mode_Error;
      end if;
      Rt_Putchar (Item);
   end Put;

   procedure Put (Item : Character) is
   begin
      Put (Current_Out.all, Item);
   end Put;

   procedure Look_Ahead
     (File        : File_Type;
      Item        : out Character;
      End_Of_Line : out Boolean)
   is
   begin
      -- Simplified - no real look-ahead on console
      Item := ' ';
      End_Of_Line := False;
   end Look_Ahead;

   procedure Look_Ahead
     (Item        : out Character;
      End_Of_Line : out Boolean)
   is
   begin
      Look_Ahead (Current_In.all, Item, End_Of_Line);
   end Look_Ahead;

   procedure Get_Immediate
     (File : File_Type;
      Item : out Character)
   is
   begin
      if not File.Is_Open or File.Mode /= In_File then
         raise Mode_Error;
      end if;
      Item := Rt_Getchar;
   end Get_Immediate;

   procedure Get_Immediate (Item : out Character) is
   begin
      Get_Immediate (Current_In.all, Item);
   end Get_Immediate;

   procedure Get_Immediate
     (File      : File_Type;
      Item      : out Character;
      Available : out Boolean)
   is
   begin
      if not File.Is_Open or File.Mode /= In_File then
         raise Mode_Error;
      end if;
      Available := Rt_Kbhit /= 0;
      if Available then
         Item := Rt_Getchar;
      else
         Item := Character'Val (0);
      end if;
   end Get_Immediate;

   procedure Get_Immediate
     (Item      : out Character;
      Available : out Boolean)
   is
   begin
      Get_Immediate (Current_In.all, Item, Available);
   end Get_Immediate;

   -- String I/O
   procedure Get (File : File_Type; Item : out String) is
   begin
      if not File.Is_Open or File.Mode /= In_File then
         raise Mode_Error;
      end if;
      for I in Item'Range loop
         Item (I) := Rt_Getchar;
      end loop;
   end Get;

   procedure Get (Item : out String) is
   begin
      Get (Current_In.all, Item);
   end Get;

   procedure Put (File : File_Type; Item : String) is
   begin
      if not File.Is_Open or File.Mode = In_File then
         raise Mode_Error;
      end if;
      for I in Item'Range loop
         Rt_Putchar (Item (I));
      end loop;
   end Put;

   procedure Put (Item : String) is
   begin
      Put (Current_Out.all, Item);
   end Put;

   procedure Get_Line
     (File : File_Type;
      Item : out String;
      Last : out Natural)
   is
      C : Character;
      I : Natural := Item'First;
   begin
      if not File.Is_Open or File.Mode /= In_File then
         raise Mode_Error;
      end if;
      Last := Item'First - 1;
      while I <= Item'Last loop
         C := Rt_Getchar;
         exit when C = Character'Val (10) or C = Character'Val (13);
         Item (I) := C;
         Last := I;
         I := I + 1;
      end loop;
      -- Skip LF if CR was received
      if C = Character'Val (13) then
         C := Rt_Getchar;  -- Consume LF
      end if;
   end Get_Line;

   procedure Get_Line
     (Item : out String;
      Last : out Natural)
   is
   begin
      Get_Line (Current_In.all, Item, Last);
   end Get_Line;

   function Get_Line (File : File_Type) return String is
      Buffer : String (1 .. 256);
      Last   : Natural;
   begin
      Get_Line (File, Buffer, Last);
      return Buffer (1 .. Last);
   end Get_Line;

   function Get_Line return String is
   begin
      return Get_Line (Current_In.all);
   end Get_Line;

   procedure Put_Line (File : File_Type; Item : String) is
   begin
      Put (File, Item);
      New_Line (File);
   end Put_Line;

   procedure Put_Line (Item : String) is
   begin
      Put_Line (Current_Out.all, Item);
   end Put_Line;

end Ada.Text_IO;
