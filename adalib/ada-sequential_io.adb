-- Ada.Sequential_IO implementation for Z80
-- Provides sequential file I/O through CP/M BDOS or direct disk access

with System;

package body Ada.Sequential_IO is

   -- Element size in bytes
   Element_Size : constant Natural := Element_Type'Size / 8;

   -- Runtime file operations (implemented in Z80 assembly)
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

   procedure Rt_File_Delete (Name : System.Address);
   pragma Import (C, Rt_File_Delete, "_file_delete");

   -- Create a new file
   procedure Create
     (File : in Out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "")
   is
      Name_Z : String := Name & Character'Val (0);  -- Null terminate
      Handle : Integer;
   begin
      if File.Is_Open then
         raise Status_Error;
      end if;

      if Mode = In_File then
         raise Use_Error;  -- Can't create input file
      end if;

      Handle := Rt_File_Create (Name_Z'Address);
      if Handle < 0 then
         raise Name_Error;
      end if;

      File.Handle := FCB_Handle (Handle);
      File.Mode := Mode;
      File.Is_Open := True;
      File.EOF := False;
   end Create;

   -- Open existing file
   procedure Open
     (File : in Out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "")
   is
      Name_Z : String := Name & Character'Val (0);
      Handle : Integer;
      Mode_Int : Integer;
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
      File.EOF := False;
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
   end Close;

   -- Delete file
   procedure Delete (File : in Out File_Type) is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;

      -- Get name before closing
      Close (File);
      -- Note: Can't delete by handle on CP/M; need name
      raise Use_Error;
   end Delete;

   -- Reset file
   procedure Reset (File : in Out File_Type; Mode : File_Mode) is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;

      -- Re-open with new mode
      -- Simplified: just update mode for sequential access
      File.Mode := Mode;
      File.EOF := False;
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
      return "FILE";  -- Simplified - would need to store name
   end Name;

   function Form (File : File_Type) return String is
   begin
      return "";
   end Form;

   function Is_Open (File : File_Type) return Boolean is
   begin
      return File.Is_Open;
   end Is_Open;

   -- Read element
   procedure Read (File : File_Type; Item : out Element_Type) is
      Bytes_Read : Integer;
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;

      if File.Mode /= In_File then
         raise Mode_Error;
      end if;

      if File.EOF then
         raise End_Error;
      end if;

      Bytes_Read := Rt_File_Read (Integer (File.Handle), Item'Address, Element_Size);

      if Bytes_Read < Element_Size then
         raise End_Error;
      end if;
   end Read;

   -- Write element
   procedure Write (File : File_Type; Item : Element_Type) is
      Bytes_Written : Integer;
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;

      if File.Mode = In_File then
         raise Mode_Error;
      end if;

      Bytes_Written := Rt_File_Write (Integer (File.Handle), Item'Address, Element_Size);

      if Bytes_Written < Element_Size then
         raise Device_Error;
      end if;
   end Write;

   -- Check for end of file
   function End_Of_File (File : File_Type) return Boolean is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;

      if File.Mode /= In_File then
         raise Mode_Error;
      end if;

      return Rt_File_EOF (Integer (File.Handle)) /= 0;
   end End_Of_File;

end Ada.Sequential_IO;
