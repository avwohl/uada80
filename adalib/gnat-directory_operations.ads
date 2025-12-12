-- GNAT.Directory_Operations for Z80
-- Directory operations (CP/M adaptation)

package GNAT.Directory_Operations is
   pragma Preelaborate;

   -- Directory handle
   type Dir_Type is limited private;
   Null_Dir : constant Dir_Type;

   -- Directory separator
   Dir_Separator : constant Character := ':';  -- CP/M uses drive letters

   -- Operations
   procedure Open (Dir : out Dir_Type; Dir_Name : String);
   procedure Close (Dir : in Out Dir_Type);
   procedure Read (Dir : Dir_Type; Str : out String; Last : out Natural);

   function Is_Open (Dir : Dir_Type) return Boolean;

   -- Current directory (drive)
   function Get_Current_Dir return String;
   procedure Change_Dir (Dir_Name : String);

   -- Path operations
   function Dir_Name (Path : String) return String;
   function Base_Name (Path : String) return String;
   function File_Extension (Path : String) return String;
   function File_Name (Path : String) return String;

   -- Exceptions
   Directory_Error : exception;

private

   type Dir_Type is record
      Is_Open    : Boolean := False;
      Drive      : Character := 'A';
      Search_Pos : Natural := 0;
   end record;

   Null_Dir : constant Dir_Type := (Is_Open => False, Drive => 'A', Search_Pos => 0);

end GNAT.Directory_Operations;
