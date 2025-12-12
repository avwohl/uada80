-- Ada.Direct_IO implementation for Z80
-- Provides random access file I/O

with System;

package body Ada.Direct_IO is

   -- Element size in bytes
   Element_Size : constant Natural := Element_Type'Size / 8;

   -- Runtime file operations
   function Rt_File_Open (Name : System.Address; Mode : Integer) return Integer;
   pragma Import (C, Rt_File_Open, "_file_open");

   function Rt_File_Create (Name : System.Address) return Integer;
   pragma Import (C, Rt_File_Create, "_file_create");

   procedure Rt_File_Close (Handle : Integer);
   pragma Import (C, Rt_File_Close, "_file_close");

   function Rt_File_Read_At
     (Handle : Integer;
      Buf    : System.Address;
      Size   : Integer;
      Pos    : Integer) return Integer;
   pragma Import (C, Rt_File_Read_At, "_file_read_at");

   function Rt_File_Write_At
     (Handle : Integer;
      Buf    : System.Address;
      Size   : Integer;
      Pos    : Integer) return Integer;
   pragma Import (C, Rt_File_Write_At, "_file_write_at");

   function Rt_File_Size (Handle : Integer) return Integer;
   pragma Import (C, Rt_File_Size, "_file_size");

   -- Calculate byte position from element index
   function Element_Position (Index : Positive_Count) return Integer is
   begin
      return Integer ((Index - 1)) * Element_Size;
   end Element_Position;

   -- Create a new file
   procedure Create
     (File : in Out File_Type;
      Mode : File_Mode := Inout_File;
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
   end Create;

   -- Open existing file
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
         when In_File    => Mode_Int := 0;
         when Out_File   => Mode_Int := 1;
         when Inout_File => Mode_Int := 2;  -- Read/write
      end case;

      Handle := Rt_File_Open (Name_Z'Address, Mode_Int);
      if Handle < 0 then
         raise Name_Error;
      end if;

      File.Handle := FCB_Handle (Handle);
      File.Mode := Mode;
      File.Is_Open := True;
      File.Index := 1;

      -- Get file size in elements
      Size_Bytes := Rt_File_Size (Handle);
      if Size_Bytes > 0 and Element_Size > 0 then
         File.File_Size := Count (Size_Bytes / Element_Size);
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
   end Close;

   -- Delete file
   procedure Delete (File : in Out File_Type) is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;
      Close (File);
      raise Use_Error;  -- Need filename for delete
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

   -- Read at specific position
   procedure Read
     (File : File_Type;
      Item : out Element_Type;
      From : Positive_Count)
   is
      Bytes_Read : Integer;
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;

      if File.Mode = Out_File then
         raise Mode_Error;
      end if;

      if From > File.File_Size then
         raise End_Error;
      end if;

      Bytes_Read := Rt_File_Read_At
        (Integer (File.Handle), Item'Address, Element_Size, Element_Position (From));

      if Bytes_Read < Element_Size then
         raise Data_Error;
      end if;
   end Read;

   -- Read at current position
   procedure Read (File : File_Type; Item : out Element_Type) is
   begin
      Read (File, Item, File.Index);
      -- Advance index (need to modify File which is in mode)
      -- Note: This is a simplification - proper impl needs variable file
   end Read;

   -- Write at specific position
   procedure Write
     (File : File_Type;
      Item : Element_Type;
      To   : Positive_Count)
   is
      Bytes_Written : Integer;
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;

      if File.Mode = In_File then
         raise Mode_Error;
      end if;

      Bytes_Written := Rt_File_Write_At
        (Integer (File.Handle), Item'Address, Element_Size, Element_Position (To));

      if Bytes_Written < Element_Size then
         raise Device_Error;
      end if;
   end Write;

   -- Write at current position
   procedure Write (File : File_Type; Item : Element_Type) is
   begin
      Write (File, Item, File.Index);
   end Write;

   -- Index operations
   procedure Set_Index (File : File_Type; To : Positive_Count) is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;
      -- Note: Would need modifiable File parameter
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

   function End_Of_File (File : File_Type) return Boolean is
   begin
      if not File.Is_Open then
         raise Status_Error;
      end if;
      return File.Index > File.File_Size;
   end End_Of_File;

end Ada.Direct_IO;
