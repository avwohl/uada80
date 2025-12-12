-- GNAT.Directory_Operations.Iteration body for Z80
-- Directory iteration implementation

package body GNAT.Directory_Operations.Iteration is

   -------------
   -- Iterate --
   -------------

   procedure Iterate (Path : Path_Name) is
      Dir   : Dir_Type;
      Item  : String (1 .. 256);
      Last  : Natural;
      Index : Positive := 1;
      Quit  : Boolean := False;
   begin
      Open (Dir, Path);

      loop
         Read (Dir, Item, Last);
         exit when Last = 0;

         Action (Item (1 .. Last), Index, Quit);
         exit when Quit;

         Index := Index + 1;
      end loop;

      Close (Dir);
   exception
      when Directory_Error =>
         null;  -- Directory doesn't exist or can't be read
   end Iterate;

   -----------------------
   -- Iterate_With_Type --
   -----------------------

   procedure Iterate_With_Type (Path : Path_Name) is
      Dir    : Dir_Type;
      Item   : String (1 .. 256);
      Last   : Natural;
      Index  : Positive := 1;
      Quit   : Boolean := False;
      Is_Dir : Boolean;
   begin
      Open (Dir, Path);

      loop
         Read (Dir, Item, Last);
         exit when Last = 0;

         -- On CP/M, assume all entries are files (no subdirectories in basic CP/M)
         Is_Dir := False;

         Action (Item (1 .. Last), Is_Dir, Index, Quit);
         exit when Quit;

         Index := Index + 1;
      end loop;

      Close (Dir);
   exception
      when Directory_Error =>
         null;
   end Iterate_With_Type;

   -----------------------
   -- Iterate_With_Path --
   -----------------------

   procedure Iterate_With_Path (Path : Path_Name) is
      Dir       : Dir_Type;
      Item      : String (1 .. 256);
      Last      : Natural;
      Index     : Positive := 1;
      Quit      : Boolean := False;
      Full_Path : String (1 .. 512);
      FP_Last   : Natural;
   begin
      Open (Dir, Path);

      loop
         Read (Dir, Item, Last);
         exit when Last = 0;

         -- Build full path
         if Path'Length > 0 and then Path (Path'Last) /= '/' then
            Full_Path (1 .. Path'Length) := Path;
            Full_Path (Path'Length + 1) := '/';
            Full_Path (Path'Length + 2 .. Path'Length + 1 + Last) := Item (1 .. Last);
            FP_Last := Path'Length + 1 + Last;
         else
            Full_Path (1 .. Path'Length) := Path;
            Full_Path (Path'Length + 1 .. Path'Length + Last) := Item (1 .. Last);
            FP_Last := Path'Length + Last;
         end if;

         Action (Item (1 .. Last), Full_Path (1 .. FP_Last), Index, Quit);
         exit when Quit;

         Index := Index + 1;
      end loop;

      Close (Dir);
   exception
      when Directory_Error =>
         null;
   end Iterate_With_Path;

end GNAT.Directory_Operations.Iteration;
