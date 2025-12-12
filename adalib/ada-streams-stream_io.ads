-- Ada.Streams.Stream_IO for Z80
-- Provides stream-based file I/O

with Ada.Streams;
with System;

package Ada.Streams.Stream_IO is
   pragma Preelaborate;

   type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;

   type File_Type is limited private;
   type File_Mode is (In_File, Out_File, Append_File);

   subtype Count is Stream_Element_Offset range 0 .. Stream_Element_Offset'Last;
   subtype Positive_Count is Count range 1 .. Count'Last;

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
   function End_Of_File (File : File_Type) return Boolean;

   -- Stream access
   function Stream (File : File_Type) return Stream_Access;

   -- Direct read/write
   procedure Read
     (File : File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset);

   procedure Write
     (File : File_Type;
      Item : Stream_Element_Array);

   -- Positioning
   procedure Set_Index (File : File_Type; To : Positive_Count);
   function Index (File : File_Type) return Positive_Count;
   function Size (File : File_Type) return Count;

   procedure Set_Mode (File : in Out File_Type; Mode : File_Mode);

   -- Exceptions (re-export from IO_Exceptions conceptually)
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

   -- File stream type for this file
   type File_Stream_Type is new Ada.Streams.Root_Stream_Type with record
      Handle : FCB_Handle := Closed_Handle;
   end record;

   overriding procedure Read
     (Stream : in Out File_Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding procedure Write
     (Stream : in Out File_Stream_Type;
      Item   : Stream_Element_Array);

   type File_Type is record
      Handle    : FCB_Handle := Closed_Handle;
      Mode      : File_Mode := In_File;
      Is_Open   : Boolean := False;
      Index     : Positive_Count := 1;
      File_Size : Count := 0;
      Stream    : aliased File_Stream_Type;
   end record;

end Ada.Streams.Stream_IO;
