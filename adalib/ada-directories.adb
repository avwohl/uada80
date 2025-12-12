-- Ada.Directories body for Z80/CP/M
-- Directory and file operations implementation

package body Ada.Directories is

   -- CP/M BDOS function numbers
   BDOS_GET_DRIVE    : constant := 25;   -- Get current drive
   BDOS_SET_DRIVE    : constant := 14;   -- Set current drive
   BDOS_SEARCH_FIRST : constant := 17;   -- Search for first match
   BDOS_SEARCH_NEXT  : constant := 18;   -- Search for next match
   BDOS_DELETE       : constant := 19;   -- Delete file
   BDOS_RENAME       : constant := 23;   -- Rename file
   BDOS_FILE_SIZE    : constant := 35;   -- Get file size

   -- External BDOS call
   procedure BDOS (Func : Natural; DE : Natural);
   pragma Import (Assembler, BDOS, "_bdos");

   function BDOS_Ret (Func : Natural; DE : Natural) return Natural;
   pragma Import (Assembler, BDOS_Ret, "_bdos_ret");

   -----------------------
   -- Current_Directory --
   -----------------------

   function Current_Directory return String is
      Drive : Natural;
   begin
      Drive := BDOS_Ret (BDOS_GET_DRIVE, 0);
      return Character'Val (Character'Pos ('A') + Drive) & ":";
   end Current_Directory;

   -------------------
   -- Set_Directory --
   -------------------

   procedure Set_Directory (Directory : String) is
      Drive : Natural;
   begin
      if Directory'Length >= 1 then
         if Directory (Directory'First) >= 'A' and Directory (Directory'First) <= 'P' then
            Drive := Character'Pos (Directory (Directory'First)) - Character'Pos ('A');
            BDOS (BDOS_SET_DRIVE, Drive);
         elsif Directory (Directory'First) >= 'a' and Directory (Directory'First) <= 'p' then
            Drive := Character'Pos (Directory (Directory'First)) - Character'Pos ('a');
            BDOS (BDOS_SET_DRIVE, Drive);
         else
            raise Name_Error;
         end if;
      else
         raise Name_Error;
      end if;
   end Set_Directory;

   ------------
   -- Exists --
   ------------

   function Exists (Name : String) return Boolean is
      -- Use search to check existence
      Search : Search_Type;
      Entry  : Directory_Entry_Type;
   begin
      Start_Search (Search, "", Name, (others => True));
      if More_Entries (Search) then
         End_Search (Search);
         return True;
      else
         End_Search (Search);
         return False;
      end if;
   exception
      when others =>
         return False;
   end Exists;

   ----------
   -- Kind --
   ----------

   function Kind (Name : String) return File_Kind is
   begin
      if Exists (Name) then
         return Ordinary_File;  -- CP/M only has files
      else
         raise Name_Error;
      end if;
   end Kind;

   ----------
   -- Size --
   ----------

   function Size (Name : String) return File_Size is
      Search : Search_Type;
      Entry  : Directory_Entry_Type;
   begin
      Start_Search (Search, "", Name, (others => True));
      if More_Entries (Search) then
         Get_Next_Entry (Search, Entry);
         End_Search (Search);
         return Entry.File_Size;
      else
         End_Search (Search);
         raise Name_Error;
      end if;
   end Size;

   ---------------
   -- Full_Name --
   ---------------

   function Full_Name (Name : String) return String is
      Drive : String := Current_Directory;
   begin
      -- If name already has drive, return as-is
      if Name'Length >= 2 and then Name (Name'First + 1) = ':' then
         return Name;
      else
         return Drive & Name;
      end if;
   end Full_Name;

   -----------------
   -- Simple_Name --
   -----------------

   function Simple_Name (Name : String) return String is
   begin
      -- If name has drive prefix, skip it
      if Name'Length >= 2 and then Name (Name'First + 1) = ':' then
         return Name (Name'First + 2 .. Name'Last);
      else
         return Name;
      end if;
   end Simple_Name;

   --------------------------
   -- Containing_Directory --
   --------------------------

   function Containing_Directory (Name : String) return String is
   begin
      -- If name has drive prefix, return it
      if Name'Length >= 2 and then Name (Name'First + 1) = ':' then
         return Name (Name'First .. Name'First + 1);
      else
         return Current_Directory;
      end if;
   end Containing_Directory;

   ---------------
   -- Extension --
   ---------------

   function Extension (Name : String) return String is
      Simple : constant String := Simple_Name (Name);
   begin
      for I in reverse Simple'Range loop
         if Simple (I) = '.' then
            return Simple (I + 1 .. Simple'Last);
         end if;
      end loop;
      return "";  -- No extension
   end Extension;

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name (Name : String) return String is
      Simple : constant String := Simple_Name (Name);
   begin
      for I in reverse Simple'Range loop
         if Simple (I) = '.' then
            return Simple (Simple'First .. I - 1);
         end if;
      end loop;
      return Simple;  -- No extension, return whole name
   end Base_Name;

   -------------
   -- Compose --
   -------------

   function Compose
     (Containing_Directory : String := "";
      Name                 : String;
      Extension            : String := "") return String
   is
   begin
      if Containing_Directory = "" then
         if Extension = "" then
            return Name;
         else
            return Name & "." & Extension;
         end if;
      else
         if Extension = "" then
            return Containing_Directory & Name;
         else
            return Containing_Directory & Name & "." & Extension;
         end if;
      end if;
   end Compose;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File (Name : String) is
      FCB : String (1 .. 36) := (others => Character'Val (0));
      Res : Natural;
   begin
      -- Build FCB for the file
      -- This is simplified - full implementation would parse drive and filename
      if not Exists (Name) then
         raise Name_Error;
      end if;

      -- Parse name into FCB format
      -- FCB byte 0 = drive (0=default, 1=A, 2=B, etc.)
      -- FCB bytes 1-8 = filename (space padded)
      -- FCB bytes 9-11 = extension (space padded)

      declare
         Simple : constant String := Simple_Name (Name);
         Base   : constant String := Base_Name (Name);
         Ext    : constant String := Extension (Name);
         Drive  : Natural := 0;  -- Default drive
      begin
         -- Set drive if specified
         if Name'Length >= 2 and then Name (Name'First + 1) = ':' then
            Drive := Character'Pos (Name (Name'First)) - Character'Pos ('A') + 1;
            if Name (Name'First) >= 'a' then
               Drive := Character'Pos (Name (Name'First)) - Character'Pos ('a') + 1;
            end if;
         end if;

         FCB (1) := Character'Val (Drive);

         -- Copy filename (up to 8 chars)
         for I in 1 .. 8 loop
            if I <= Base'Length then
               FCB (1 + I) := Base (Base'First + I - 1);
            else
               FCB (1 + I) := ' ';
            end if;
         end loop;

         -- Copy extension (up to 3 chars)
         for I in 1 .. 3 loop
            if I <= Ext'Length then
               FCB (9 + I) := Ext (Ext'First + I - 1);
            else
               FCB (9 + I) := ' ';
            end if;
         end loop;
      end;

      -- Call BDOS delete function
      -- Note: In real implementation, would pass FCB address
      Res := BDOS_Ret (BDOS_DELETE, 0);
      if Res = 255 then
         raise Use_Error;
      end if;
   end Delete_File;

   ------------
   -- Rename --
   ------------

   procedure Rename (Old_Name, New_Name : String) is
   begin
      if not Exists (Old_Name) then
         raise Name_Error;
      end if;

      if Exists (New_Name) then
         raise Use_Error;  -- Target already exists
      end if;

      -- CP/M rename requires special FCB format
      -- This is a stub - full implementation would use BDOS function 23
      raise Use_Error;
   end Rename;

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File (Source_Name, Target_Name : String) is
   begin
      if not Exists (Source_Name) then
         raise Name_Error;
      end if;

      -- Copy requires reading source and writing to target
      -- This is a stub - full implementation would use file I/O
      raise Use_Error;
   end Copy_File;

   ------------------
   -- Start_Search --
   ------------------

   procedure Start_Search
     (Search    : in Out Search_Type;
      Directory : String;
      Pattern   : String;
      Filter    : Filter_Type := (others => True))
   is
   begin
      Search.First_Call := True;
      Search.Has_More := False;
      Search.Filter := Filter;

      -- Store pattern
      Search.Pat_Len := Natural'Min (Pattern'Length, Max_Filename_Length);
      for I in 1 .. Search.Pat_Len loop
         Search.Pattern (I) := Pattern (Pattern'First + I - 1);
      end loop;

      -- Determine drive
      if Directory'Length >= 1 then
         Search.Drive := Directory (Directory'First);
      elsif Pattern'Length >= 2 and then Pattern (Pattern'First + 1) = ':' then
         Search.Drive := Pattern (Pattern'First);
      else
         Search.Drive := Current_Directory (1);
      end if;

      -- Initialize FCB for search
      -- Will be populated on first More_Entries call
   end Start_Search;

   ----------------
   -- End_Search --
   ----------------

   procedure End_Search (Search : in Out Search_Type) is
   begin
      Search.Has_More := False;
      Search.First_Call := True;
   end End_Search;

   ------------------
   -- More_Entries --
   ------------------

   function More_Entries (Search : Search_Type) return Boolean is
   begin
      -- In a real implementation, this would call BDOS search functions
      -- For now, return based on state
      return Search.Has_More;
   end More_Entries;

   --------------------
   -- Get_Next_Entry --
   --------------------

   procedure Get_Next_Entry
     (Search          : in Out Search_Type;
      Directory_Entry : out Directory_Entry_Type)
   is
   begin
      if not Search.Has_More then
         raise Status_Error;
      end if;

      -- Fill entry from search results
      -- This is a stub - real implementation would parse DMA buffer
      Directory_Entry.Valid := False;
      Search.Has_More := False;
   end Get_Next_Entry;

   -- Directory_Entry_Type operations

   function Simple_Name (Directory_Entry : Directory_Entry_Type) return String is
   begin
      if not Directory_Entry.Valid then
         raise Status_Error;
      end if;
      return Directory_Entry.Name (1 .. Directory_Entry.Name_Len);
   end Simple_Name;

   function Full_Name (Directory_Entry : Directory_Entry_Type) return String is
   begin
      if not Directory_Entry.Valid then
         raise Status_Error;
      end if;
      return Directory_Entry.Drive & ":" &
             Directory_Entry.Name (1 .. Directory_Entry.Name_Len);
   end Full_Name;

   function Kind (Directory_Entry : Directory_Entry_Type) return File_Kind is
   begin
      if not Directory_Entry.Valid then
         raise Status_Error;
      end if;
      return Ordinary_File;  -- CP/M only has files
   end Kind;

   function Size (Directory_Entry : Directory_Entry_Type) return File_Size is
   begin
      if not Directory_Entry.Valid then
         raise Status_Error;
      end if;
      return Directory_Entry.File_Size;
   end Size;

end Ada.Directories;
