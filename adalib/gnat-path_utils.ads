-- GNAT.Path_Utils for Z80/CP/M
-- File path manipulation utilities for CP/M filesystem

package GNAT.Path_Utils is
   pragma Pure;

   Max_Path_Length : constant := 64;
   Max_Name_Length : constant := 8;   -- CP/M filename limit
   Max_Ext_Length  : constant := 3;   -- CP/M extension limit

   subtype Path_String is String (1 .. Max_Path_Length);

   -- Path component extraction
   function Get_Drive (Path : String) return Character;
   -- Returns drive letter (A-P) or ' ' if none

   function Get_Directory (Path : String) return String;
   -- Returns user number prefix if present (e.g., "0:")

   function Get_Filename (Path : String) return String;
   -- Returns filename without extension

   function Get_Extension (Path : String) return String;
   -- Returns extension without dot

   function Get_Basename (Path : String) return String;
   -- Returns filename with extension

   -- Path construction
   function Make_Path (Drive : Character;
                       Name : String;
                       Ext : String := "") return String;

   function Set_Extension (Path : String; Ext : String) return String;
   -- Replace or add extension

   function Remove_Extension (Path : String) return String;
   -- Remove extension from path

   -- Path validation
   function Is_Valid_Filename (Name : String) return Boolean;
   function Is_Valid_Extension (Ext : String) return Boolean;
   function Is_Valid_Path (Path : String) return Boolean;

   -- CP/M specific
   function Has_Drive (Path : String) return Boolean;
   function Has_Extension (Path : String) return Boolean;
   function Has_User_Number (Path : String) return Boolean;

   function Get_User_Number (Path : String) return Natural;
   -- Returns user number (0-15) or 0 if not specified

   function Make_FCB_Name (Path : String) return String;
   -- Convert to FCB format (8.3, uppercase, space-padded)

   -- Wildcard handling
   function Has_Wildcards (Path : String) return Boolean;
   function Is_Wildcard (C : Character) return Boolean;
   function Match_Pattern (Name, Pattern : String) return Boolean;

   -- Case conversion (CP/M is case-insensitive)
   function To_Upper_Path (Path : String) return String;

   -- Comparison (case-insensitive)
   function Same_Path (Path1, Path2 : String) return Boolean;
   function Same_Name (Name1, Name2 : String) return Boolean;

   -- Standard extensions
   function Is_Executable (Path : String) return Boolean;  -- .COM
   function Is_Command (Path : String) return Boolean;     -- .COM or .SUB
   function Is_Text (Path : String) return Boolean;        -- .TXT, .DOC, etc.
   function Is_Data (Path : String) return Boolean;        -- .DAT
   function Is_Source (Path : String) return Boolean;      -- .ADA, .ASM, etc.

   -- Special filenames
   function Is_Console (Path : String) return Boolean;     -- CON:
   function Is_Printer (Path : String) return Boolean;     -- LST:
   function Is_Reader (Path : String) return Boolean;      -- RDR:
   function Is_Punch (Path : String) return Boolean;       -- PUN:

end GNAT.Path_Utils;
