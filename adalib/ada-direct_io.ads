-- Ada.Direct_IO generic package for Z80
-- Random access file I/O for fixed-size elements
--
-- Provides indexed read/write access to file records.
-- On Z80/CP/M, uses 128-byte sector-based access.

generic
   type Element_Type is private;

package Ada.Direct_IO is
   pragma Preelaborate;

   type File_Type is limited private;
   type File_Mode is (In_File, Inout_File, Out_File);

   -- Record count type (16-bit for Z80)
   type Count is range 0 .. 65535;
   subtype Positive_Count is Count range 1 .. Count'Last;

   -- File management
   procedure Create
     (File : in Out File_Type;
      Mode : File_Mode := Inout_File;
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

   -- Direct (indexed) operations
   procedure Read
     (File : File_Type;
      Item : out Element_Type;
      From : Positive_Count);

   procedure Read (File : File_Type; Item : out Element_Type);

   procedure Write
     (File : File_Type;
      Item : Element_Type;
      To   : Positive_Count);

   procedure Write (File : File_Type; Item : Element_Type);

   procedure Set_Index (File : File_Type; To : Positive_Count);
   function Index (File : File_Type) return Positive_Count;
   function Size (File : File_Type) return Count;

   function End_Of_File (File : File_Type) return Boolean;

   -- Exceptions
   Status_Error : exception;
   Mode_Error   : exception;
   Name_Error   : exception;
   Use_Error    : exception;
   Device_Error : exception;
   End_Error    : exception;
   Data_Error   : exception;

private

   type FCB_Handle is range 0 .. 255;
   Closed_Handle : constant FCB_Handle := 255;

   type File_Type is record
      Handle    : FCB_Handle := Closed_Handle;
      Mode      : File_Mode := In_File;
      Is_Open   : Boolean := False;
      Index     : Positive_Count := 1;
      File_Size : Count := 0;
   end record;

end Ada.Direct_IO;
