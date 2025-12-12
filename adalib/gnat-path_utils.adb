-- GNAT.Path_Utils body for Z80/CP/M
-- Path manipulation implementation

package body GNAT.Path_Utils is

   function To_Upper (C : Character) return Character is
   begin
      if C >= 'a' and C <= 'z' then
         return Character'Val (Character'Pos (C) - 32);
      else
         return C;
      end if;
   end To_Upper;

   function Is_Alpha (C : Character) return Boolean is
   begin
      return (C >= 'A' and C <= 'Z') or (C >= 'a' and C <= 'z');
   end Is_Alpha;

   function Is_Digit (C : Character) return Boolean is
   begin
      return C >= '0' and C <= '9';
   end Is_Digit;

   function Is_Valid_Char (C : Character) return Boolean is
   begin
      -- Valid CP/M filename characters
      return Is_Alpha (C) or Is_Digit (C) or
             C = '_' or C = '-' or C = '$' or C = '!' or
             C = '#' or C = '%' or C = '&' or C = '(' or
             C = ')' or C = '@' or C = '^' or C = '`' or
             C = '{' or C = '}' or C = '~';
   end Is_Valid_Char;

   ---------------
   -- Get_Drive --
   ---------------

   function Get_Drive (Path : String) return Character is
   begin
      if Path'Length >= 2 and then Path (Path'First + 1) = ':' then
         declare
            D : constant Character := To_Upper (Path (Path'First));
         begin
            if D >= 'A' and D <= 'P' then
               return D;
            end if;
         end;
      end if;
      return ' ';
   end Get_Drive;

   -------------------
   -- Get_Directory --
   -------------------

   function Get_Directory (Path : String) return String is
      Start : Natural := Path'First;
   begin
      -- Check for user number format: N: where N is 0-15
      if Path'Length >= 2 and then Is_Digit (Path (Path'First)) then
         if Path (Path'First + 1) = ':' then
            return Path (Path'First .. Path'First + 1);
         elsif Path'Length >= 3 and then Is_Digit (Path (Path'First + 1)) and then
               Path (Path'First + 2) = ':' then
            return Path (Path'First .. Path'First + 2);
         end if;
      end if;

      -- Check for drive letter
      if Has_Drive (Path) then
         return Path (Path'First .. Path'First + 1);
      end if;

      return "";
   end Get_Directory;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Path : String) return String is
      Start     : Natural := Path'First;
      Dot_Pos   : Natural := 0;
   begin
      -- Skip drive/user prefix
      if Has_Drive (Path) then
         Start := Path'First + 2;
      elsif Has_User_Number (Path) then
         Start := Path'First + 2;
         if Path'Length > 2 and then Is_Digit (Path (Path'First + 1)) then
            Start := Path'First + 3;
         end if;
      end if;

      -- Find dot position
      for I in Start .. Path'Last loop
         if Path (I) = '.' then
            Dot_Pos := I;
            exit;
         end if;
      end loop;

      if Dot_Pos > 0 then
         return Path (Start .. Dot_Pos - 1);
      else
         return Path (Start .. Path'Last);
      end if;
   end Get_Filename;

   -------------------
   -- Get_Extension --
   -------------------

   function Get_Extension (Path : String) return String is
   begin
      for I in reverse Path'Range loop
         if Path (I) = '.' then
            if I < Path'Last then
               return Path (I + 1 .. Path'Last);
            else
               return "";
            end if;
         elsif Path (I) = ':' then
            -- Hit drive separator before dot
            return "";
         end if;
      end loop;
      return "";
   end Get_Extension;

   ------------------
   -- Get_Basename --
   ------------------

   function Get_Basename (Path : String) return String is
      Start : Natural := Path'First;
   begin
      -- Skip drive/user prefix
      if Has_Drive (Path) then
         Start := Path'First + 2;
      elsif Has_User_Number (Path) then
         Start := Path'First + 2;
         if Path'Length > 2 and then Is_Digit (Path (Path'First + 1)) then
            Start := Path'First + 3;
         end if;
      end if;

      return Path (Start .. Path'Last);
   end Get_Basename;

   ---------------
   -- Make_Path --
   ---------------

   function Make_Path (Drive : Character;
                       Name : String;
                       Ext : String := "") return String is
      Result : String (1 .. Max_Path_Length);
      Pos    : Natural := 1;
   begin
      Result := (others => ' ');

      -- Add drive if specified
      if Drive /= ' ' then
         Result (Pos) := To_Upper (Drive);
         Pos := Pos + 1;
         Result (Pos) := ':';
         Pos := Pos + 1;
      end if;

      -- Add filename (up to 8 chars)
      for I in Name'Range loop
         exit when Pos > Max_Path_Length;
         exit when Pos > (if Drive /= ' ' then 10 else 8);
         Result (Pos) := To_Upper (Name (I));
         Pos := Pos + 1;
      end loop;

      -- Add extension if specified
      if Ext'Length > 0 then
         if Pos <= Max_Path_Length then
            Result (Pos) := '.';
            Pos := Pos + 1;
         end if;
         for I in Ext'Range loop
            exit when Pos > Max_Path_Length;
            exit when I - Ext'First >= 3;  -- Max 3 chars
            Result (Pos) := To_Upper (Ext (I));
            Pos := Pos + 1;
         end loop;
      end if;

      return Result (1 .. Pos - 1);
   end Make_Path;

   -------------------
   -- Set_Extension --
   -------------------

   function Set_Extension (Path : String; Ext : String) return String is
      Base : constant String := Remove_Extension (Path);
   begin
      if Ext'Length > 0 then
         return Base & "." & Ext;
      else
         return Base;
      end if;
   end Set_Extension;

   ----------------------
   -- Remove_Extension --
   ----------------------

   function Remove_Extension (Path : String) return String is
   begin
      for I in reverse Path'Range loop
         if Path (I) = '.' then
            return Path (Path'First .. I - 1);
         elsif Path (I) = ':' then
            -- Hit drive separator before dot
            return Path;
         end if;
      end loop;
      return Path;
   end Remove_Extension;

   -----------------------
   -- Is_Valid_Filename --
   -----------------------

   function Is_Valid_Filename (Name : String) return Boolean is
   begin
      if Name'Length = 0 or Name'Length > Max_Name_Length then
         return False;
      end if;

      for C of Name loop
         if not Is_Valid_Char (C) and C /= '?' and C /= '*' then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_Filename;

   ------------------------
   -- Is_Valid_Extension --
   ------------------------

   function Is_Valid_Extension (Ext : String) return Boolean is
   begin
      if Ext'Length > Max_Ext_Length then
         return False;
      end if;

      for C of Ext loop
         if not Is_Valid_Char (C) and C /= '?' and C /= '*' then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_Extension;

   -------------------
   -- Is_Valid_Path --
   -------------------

   function Is_Valid_Path (Path : String) return Boolean is
   begin
      if Path'Length = 0 then
         return False;
      end if;

      return Is_Valid_Filename (Get_Filename (Path)) and
             Is_Valid_Extension (Get_Extension (Path));
   end Is_Valid_Path;

   ---------------
   -- Has_Drive --
   ---------------

   function Has_Drive (Path : String) return Boolean is
   begin
      if Path'Length >= 2 and then Path (Path'First + 1) = ':' then
         declare
            D : constant Character := To_Upper (Path (Path'First));
         begin
            return D >= 'A' and D <= 'P';
         end;
      end if;
      return False;
   end Has_Drive;

   -------------------
   -- Has_Extension --
   -------------------

   function Has_Extension (Path : String) return Boolean is
   begin
      for I in reverse Path'Range loop
         if Path (I) = '.' then
            return I < Path'Last;
         elsif Path (I) = ':' then
            return False;
         end if;
      end loop;
      return False;
   end Has_Extension;

   ---------------------
   -- Has_User_Number --
   ---------------------

   function Has_User_Number (Path : String) return Boolean is
   begin
      if Path'Length >= 2 and then Is_Digit (Path (Path'First)) then
         if Path (Path'First + 1) = ':' then
            return True;
         elsif Path'Length >= 3 and then
               Is_Digit (Path (Path'First + 1)) and then
               Path (Path'First + 2) = ':' then
            return True;
         end if;
      end if;
      return False;
   end Has_User_Number;

   ---------------------
   -- Get_User_Number --
   ---------------------

   function Get_User_Number (Path : String) return Natural is
      N : Natural := 0;
   begin
      if Path'Length >= 2 and then Is_Digit (Path (Path'First)) then
         N := Character'Pos (Path (Path'First)) - Character'Pos ('0');
         if Path (Path'First + 1) = ':' then
            return N;
         elsif Path'Length >= 3 and then
               Is_Digit (Path (Path'First + 1)) and then
               Path (Path'First + 2) = ':' then
            N := N * 10 + Character'Pos (Path (Path'First + 1)) -
                 Character'Pos ('0');
            if N <= 15 then
               return N;
            end if;
         end if;
      end if;
      return 0;
   end Get_User_Number;

   -------------------
   -- Make_FCB_Name --
   -------------------

   function Make_FCB_Name (Path : String) return String is
      Result   : String (1 .. 11) := (others => ' ');
      Name     : constant String := Get_Filename (Path);
      Ext      : constant String := Get_Extension (Path);
      Pos      : Natural := 1;
   begin
      -- Copy name (8 chars, space-padded)
      for C of Name loop
         exit when Pos > 8;
         Result (Pos) := To_Upper (C);
         Pos := Pos + 1;
      end loop;

      -- Copy extension (3 chars, space-padded)
      Pos := 9;
      for C of Ext loop
         exit when Pos > 11;
         Result (Pos) := To_Upper (C);
         Pos := Pos + 1;
      end loop;

      return Result;
   end Make_FCB_Name;

   -------------------
   -- Has_Wildcards --
   -------------------

   function Has_Wildcards (Path : String) return Boolean is
   begin
      for C of Path loop
         if C = '?' or C = '*' then
            return True;
         end if;
      end loop;
      return False;
   end Has_Wildcards;

   -----------------
   -- Is_Wildcard --
   -----------------

   function Is_Wildcard (C : Character) return Boolean is
   begin
      return C = '?' or C = '*';
   end Is_Wildcard;

   -------------------
   -- Match_Pattern --
   -------------------

   function Match_Pattern (Name, Pattern : String) return Boolean is
      NI : Natural := Name'First;
      PI : Natural := Pattern'First;
   begin
      while PI <= Pattern'Last loop
         if Pattern (PI) = '*' then
            -- Match any remaining characters
            return True;
         elsif NI > Name'Last then
            return False;
         elsif Pattern (PI) = '?' or else
               To_Upper (Pattern (PI)) = To_Upper (Name (NI)) then
            NI := NI + 1;
            PI := PI + 1;
         else
            return False;
         end if;
      end loop;

      return NI > Name'Last;
   end Match_Pattern;

   -------------------
   -- To_Upper_Path --
   -------------------

   function To_Upper_Path (Path : String) return String is
      Result : String (Path'Range);
   begin
      for I in Path'Range loop
         Result (I) := To_Upper (Path (I));
      end loop;
      return Result;
   end To_Upper_Path;

   ---------------
   -- Same_Path --
   ---------------

   function Same_Path (Path1, Path2 : String) return Boolean is
   begin
      return To_Upper_Path (Path1) = To_Upper_Path (Path2);
   end Same_Path;

   ---------------
   -- Same_Name --
   ---------------

   function Same_Name (Name1, Name2 : String) return Boolean is
   begin
      return To_Upper_Path (Name1) = To_Upper_Path (Name2);
   end Same_Name;

   -------------------
   -- Is_Executable --
   -------------------

   function Is_Executable (Path : String) return Boolean is
      Ext : constant String := Get_Extension (Path);
   begin
      return Same_Name (Ext, "COM");
   end Is_Executable;

   ----------------
   -- Is_Command --
   ----------------

   function Is_Command (Path : String) return Boolean is
      Ext : constant String := Get_Extension (Path);
   begin
      return Same_Name (Ext, "COM") or Same_Name (Ext, "SUB");
   end Is_Command;

   -------------
   -- Is_Text --
   -------------

   function Is_Text (Path : String) return Boolean is
      Ext : constant String := Get_Extension (Path);
   begin
      return Same_Name (Ext, "TXT") or Same_Name (Ext, "DOC") or
             Same_Name (Ext, "ASM") or Same_Name (Ext, "ADA") or
             Same_Name (Ext, "BAS") or Same_Name (Ext, "PAS");
   end Is_Text;

   -------------
   -- Is_Data --
   -------------

   function Is_Data (Path : String) return Boolean is
      Ext : constant String := Get_Extension (Path);
   begin
      return Same_Name (Ext, "DAT") or Same_Name (Ext, "DB");
   end Is_Data;

   ---------------
   -- Is_Source --
   ---------------

   function Is_Source (Path : String) return Boolean is
      Ext : constant String := Get_Extension (Path);
   begin
      return Same_Name (Ext, "ADA") or Same_Name (Ext, "ASM") or
             Same_Name (Ext, "C") or Same_Name (Ext, "PAS") or
             Same_Name (Ext, "BAS") or Same_Name (Ext, "FOR");
   end Is_Source;

   ----------------
   -- Is_Console --
   ----------------

   function Is_Console (Path : String) return Boolean is
   begin
      return Same_Path (Path, "CON:") or Same_Path (Path, "CON");
   end Is_Console;

   ----------------
   -- Is_Printer --
   ----------------

   function Is_Printer (Path : String) return Boolean is
   begin
      return Same_Path (Path, "LST:") or Same_Path (Path, "LST") or
             Same_Path (Path, "PRN:") or Same_Path (Path, "PRN");
   end Is_Printer;

   ---------------
   -- Is_Reader --
   ---------------

   function Is_Reader (Path : String) return Boolean is
   begin
      return Same_Path (Path, "RDR:") or Same_Path (Path, "RDR");
   end Is_Reader;

   --------------
   -- Is_Punch --
   --------------

   function Is_Punch (Path : String) return Boolean is
   begin
      return Same_Path (Path, "PUN:") or Same_Path (Path, "PUN");
   end Is_Punch;

end GNAT.Path_Utils;
