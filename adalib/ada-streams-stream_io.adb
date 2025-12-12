-- Ada.Streams.Stream_IO body for Z80
-- Stream-based file I/O implementation

with System;

package body Ada.Streams.Stream_IO is

   -- Runtime file operations
   function Rt_File_Open (Name : System.Address; Mode : Integer) return Integer;
   pragma Import (C, Rt_File_Open, "_file_open");

   function Rt_File_Create (Name : System.Address) return Integer;
   pragma Import (C, Rt_File_Create, "_file_create");

   procedure Rt_File_Close (Handle : Integer);
   pragma Import (C, Rt_File_Close, "_file_close");

   function Rt_File_Read (Handle : Integer; Buf : System.Address; Size : Integer) return Integer;
   pragma Import (C, Rt_File_Read, "_file_read");

   function Rt_File_Write (Handle : Integer; Buf : System.Address; Size : Integer) return Integer;
   pragma Import (C, Rt_File_Write, "_file_write");

   function Rt_File_EOF (Handle : Integer) return Integer;
   pragma Import (C, Rt_File_EOF, "_file_eof");

   function Rt_File_Size (Handle : Integer) return Integer;
   pragma Import (C, Rt_File_Size, "_file_size");

   -- Create file
   procedure Create
     (File : in Out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "")
   is
      Name_Z : String := Name & Character'Val (0);
      Handle : Integer;
   begin
      if File.Is_Open then
         raise Status_Error;
      end if;

      Handle := Rt_File_Create (Name_Z'Address);
      if Handle < 0 then
         raise Name_Error;
      end if;

      File.Handle := FCB_Handle (Handle);
      File.Mode := Mode;
      File.Is_Open := True;
      File.Index := 1;
      File.File_Size := 0;
      File.Stream.Handle := File.Handle;
   end Create;

   -- Open file
   procedure Open
     (File : in Out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "")
   is
      Name_Z   : String := Name & Character'Val (0);
      Handle   : Integer;
      Mode_Int : Integer;
      Size_Bytes : Integer;
   begin
      if File.Is_Open then
         raise Status_Error;
      end if;

      case Mode is
         when In_File     => Mode_Int := 0;
         when Out_File    => Mode_Int := 1;
         when Append_File => Mode_Int := 2;
      end case;

      Handle := Rt_File_Open (Name_Z'Address, Mode_Int);
      if Handle < 0 then
         raise Name_Error;
      end if;

      File.Handle := FCB_Handle (Handle);
      File.Mode := Mode;
      File.Is_Open := True;
      File.Index := 1;
      File.Stream.Handle := File.Handle;

      -- Get file size
      Size_Bytes := Rt_File_Size (Handle);
      if Size_Bytes > 0 then
         File.File_Size := Count (Size_Bytes);
      else
         File.File_Size := 0;
      end if;
   end Open;

   -- Close file
   procedure Close (File : in Out File_Type) is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;

      Rt_File_Close (Integer (File.Handle));
      File.Handle := Closed_Handle;
      File.Is_Open := False;
      File.Stream.Handle := Closed_Handle;
   end Close;

   -- Delete file
   procedure Delete (File : in Out File_Type) is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;
      Close (File);
      raise Use_Error;  -- Need filename
   end Delete;

   -- Reset file
   procedure Reset (File : in Out File_Type; Mode : File_Mode) is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;
      File.Mode := Mode;
      File.Index := 1;
   end Reset;

   procedure Reset (File : in Out File_Type) is
   begin
      Reset (File, File.Mode);
   end Reset;

   -- Query functions
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
      return "FILE";
   end Name;

   function Form (File : File_Type) return String is
   begin
      return "";
   end Form;

   function Is_Open (File : File_Type) return Boolean is
   begin
      return File.Is_Open;
   end Is_Open;

   function End_Of_File (File : File_Type) return Boolean is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;
      return Rt_File_EOF (Integer (File.Handle)) /= 0;
   end End_Of_File;

   -- Stream access
   function Stream (File : File_Type) return Stream_Access is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;
      return File.Stream'Unchecked_Access;
   end Stream;

   -- Direct read
   procedure Read
     (File : File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset)
   is
      Bytes_Read : Integer;
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;

      if File.Mode = Out_File then
         raise Mode_Error;
      end if;

      Bytes_Read := Rt_File_Read
        (Integer (File.Handle), Item'Address, Item'Length);

      if Bytes_Read > 0 then
         Last := Item'First + Stream_Element_Offset (Bytes_Read) - 1;
      else
         Last := Item'First - 1;
      end if;
   end Read;

   -- Direct write
   procedure Write
     (File : File_Type;
      Item : Stream_Element_Array)
   is
      Bytes_Written : Integer;
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;

      if File.Mode = In_File then
         raise Mode_Error;
      end if;

      Bytes_Written := Rt_File_Write
        (Integer (File.Handle), Item'Address, Item'Length);

      if Bytes_Written < Item'Length then
         raise Device_Error;
      end if;
   end Write;

   -- Positioning
   procedure Set_Index (File : File_Type; To : Positive_Count) is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;
      -- Note: Would need runtime support for seeking
   end Set_Index;

   function Index (File : File_Type) return Positive_Count is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;
      return File.Index;
   end Index;

   function Size (File : File_Type) return Count is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;
      return File.File_Size;
   end Size;

   procedure Set_Mode (File : in Out File_Type; Mode : File_Mode) is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;
      File.Mode := Mode;
   end Set_Mode;

   -- File_Stream_Type implementations
   overriding procedure Read
     (Stream : in Out File_Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Bytes_Read : Integer;
   begin
      Bytes_Read := Rt_File_Read
        (Integer (Stream.Handle), Item'Address, Item'Length);

      if Bytes_Read > 0 then
         Last := Item'First + Stream_Element_Offset (Bytes_Read) - 1;
      else
         Last := Item'First - 1;
      end if;
   end Read;

   overriding procedure Write
     (Stream : in Out File_Stream_Type;
      Item   : Stream_Element_Array)
   is
      Bytes_Written : Integer;
   begin
      Bytes_Written := Rt_File_Write
        (Integer (Stream.Handle), Item'Address, Item'Length);
   end Write;

end Ada.Streams.Stream_IO;
