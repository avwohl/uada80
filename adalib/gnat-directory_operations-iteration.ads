-- GNAT.Directory_Operations.Iteration for Z80
-- Directory iteration support

package GNAT.Directory_Operations.Iteration is

   generic
      with procedure Action
        (Item  : String;
         Index : Positive;
         Quit  : in Out Boolean);
   procedure Iterate (Path : Path_Name);
   --  Iterate over directory entries

   generic
      with procedure Action
        (Item    : String;
         Is_Dir  : Boolean;
         Index   : Positive;
         Quit    : in Out Boolean);
   procedure Iterate_With_Type (Path : Path_Name);
   --  Iterate with file type information

   generic
      with procedure Action
        (Item       : String;
         Full_Path  : String;
         Index      : Positive;
         Quit       : in Out Boolean);
   procedure Iterate_With_Path (Path : Path_Name);
   --  Iterate with full path

end GNAT.Directory_Operations.Iteration;
