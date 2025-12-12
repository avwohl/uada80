-- Ada.Sequential_IO generic package for Z80
-- Sequential file I/O for fixed-size elements
--
-- On Z80/CP/M systems, this interfaces with FCB-based file operations.
-- Limited implementation for embedded systems with file system support.

generic
   type Element_Type is private;

package Ada.Sequential_IO is
   pragma Preelaborate;

   type File_Type is limited private;
   type File_Mode is (In_File, Out_File, Append_File);

   -- File management
   procedure Create
     (File : in out File_Type;
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

   -- Sequential operations
   procedure Read (File : File_Type; Item : out Element_Type);
   procedure Write (File : File_Type; Item : Element_Type);

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

   -- File Control Block handle (CP/M style)
   type FCB_Handle is range 0 .. 255;
   Closed_Handle : constant FCB_Handle := 255;

   type File_Type is record
      Handle  : FCB_Handle := Closed_Handle;
      Mode    : File_Mode := In_File;
      Is_Open : Boolean := False;
      EOF     : Boolean := False;
   end record;

end Ada.Sequential_IO;
