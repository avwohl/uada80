-- GNAT.Directory_Operations body for Z80
-- Directory operations (CP/M adaptation)

package body GNAT.Directory_Operations is

   ----------
   -- Open --
   ----------

   procedure Open (Dir : out Dir_Type; Dir_Name : String) is
   begin
      Dir.Is_Open := True;
      Dir.Search_Pos := 0;

      -- Extract drive letter
      if Dir_Name'Length >= 2 and then Dir_Name (Dir_Name'First + 1) = ':' then
         Dir.Drive := Dir_Name (Dir_Name'First);
      else
         Dir.Drive := 'A';
      end if;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Dir : in Out Dir_Type) is
   begin
      Dir.Is_Open := False;
   end Close;

   ----------
   -- Read --
   ----------

   procedure Read (Dir : Dir_Type; Str : out String; Last : out Natural) is
   begin
      -- CP/M directory reading would use Search_First/Search_Next
      -- This is a simplified stub
      Last := 0;
      Str := (others => ' ');

      if not Dir.Is_Open then
         raise Directory_Error;
      end if;

      -- Would iterate through directory entries
      -- For now, return empty
   end Read;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Dir : Dir_Type) return Boolean is
   begin
      return Dir.Is_Open;
   end Is_Open;

   ---------------------
   -- Get_Current_Dir --
   ---------------------

   function Get_Current_Dir return String is
   begin
      return "A:";  -- Default CP/M drive
   end Get_Current_Dir;

   ----------------
   -- Change_Dir --
   ----------------

   procedure Change_Dir (Dir_Name : String) is
      pragma Unreferenced (Dir_Name);
   begin
      -- Would select drive via BDOS
      null;
   end Change_Dir;

   --------------
   -- Dir_Name --
   --------------

   function Dir_Name (Path : String) return String is
   begin
      -- Return drive letter portion
      if Path'Length >= 2 and then Path (Path'First + 1) = ':' then
         return Path (Path'First .. Path'First + 1);
      else
         return "";
      end if;
   end Dir_Name;

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name (Path : String) return String is
      Start : Natural := Path'First;
      Stop  : Natural := Path'Last;
   begin
      -- Skip drive letter
      if Path'Length >= 2 and then Path (Path'First + 1) = ':' then
         Start := Path'First + 2;
      end if;

      -- Find extension
      for I in reverse Start .. Path'Last loop
         if Path (I) = '.' then
            Stop := I - 1;
            exit;
         end if;
      end loop;

      if Start > Stop then
         return "";
      end if;

      return Path (Start .. Stop);
   end Base_Name;

   --------------------
   -- File_Extension --
   --------------------

   function File_Extension (Path : String) return String is
   begin
      for I in reverse Path'Range loop
         if Path (I) = '.' then
            return Path (I .. Path'Last);
         end if;
      end loop;
      return "";
   end File_Extension;

   ---------------
   -- File_Name --
   ---------------

   function File_Name (Path : String) return String is
      Start : Natural := Path'First;
   begin
      -- Skip drive letter
      if Path'Length >= 2 and then Path (Path'First + 1) = ':' then
         Start := Path'First + 2;
      end if;

      if Start > Path'Last then
         return "";
      end if;

      return Path (Start .. Path'Last);
   end File_Name;

end GNAT.Directory_Operations;
