-- GNAT.Version for Z80
-- Version information and comparison utilities

package GNAT.Version is
   pragma Preelaborate;

   -- Version record
   type Version_Number is record
      Major : Natural;
      Minor : Natural;
      Patch : Natural;
   end record;

   -- Create version from components
   function Make_Version (Major, Minor : Natural;
                          Patch : Natural := 0) return Version_Number;

   -- Parse version from string (e.g., "1.2.3")
   function Parse_Version (S : String) return Version_Number;

   -- Format version as string
   function To_String (V : Version_Number) return String;
   function To_String_Short (V : Version_Number) return String;  -- "1.2"

   -- Comparison
   function "=" (Left, Right : Version_Number) return Boolean;
   function "<" (Left, Right : Version_Number) return Boolean;
   function "<=" (Left, Right : Version_Number) return Boolean;
   function ">" (Left, Right : Version_Number) return Boolean;
   function ">=" (Left, Right : Version_Number) return Boolean;

   -- Check compatibility
   function Is_Compatible (Required, Actual : Version_Number) return Boolean;
   function Is_Newer (V1, V2 : Version_Number) return Boolean;
   function Is_Same_Major (V1, V2 : Version_Number) return Boolean;

   -- Increment versions
   function Increment_Major (V : Version_Number) return Version_Number;
   function Increment_Minor (V : Version_Number) return Version_Number;
   function Increment_Patch (V : Version_Number) return Version_Number;

   -- Compiler version
   Compiler_Version : constant Version_Number := (Major => 1, Minor => 0, Patch => 0);
   Compiler_Name    : constant String := "UADA80";

   -- Runtime version
   Runtime_Version : constant Version_Number := (Major => 1, Minor => 0, Patch => 0);

   -- Build information
   type Build_Info is record
      Version    : Version_Number;
      Build_Date : String (1 .. 10);  -- "YYYY-MM-DD"
      Target     : String (1 .. 8);   -- "Z80-CPM"
   end record;

   Current_Build : constant Build_Info := (
      Version    => (Major => 1, Minor => 0, Patch => 0),
      Build_Date => "2025-01-01",
      Target     => "Z80-CPM "
   );

   -- Get build info as string
   function Build_String return String;

   -- Version range check
   function In_Range (V, Min_Version, Max_Version : Version_Number) return Boolean;

   -- Semantic versioning helpers
   function Is_Breaking_Change (Old_V, New_V : Version_Number) return Boolean;
   function Is_Feature_Release (Old_V, New_V : Version_Number) return Boolean;
   function Is_Bug_Fix (Old_V, New_V : Version_Number) return Boolean;

   -- Format helpers
   function Major_String (V : Version_Number) return String;
   function Minor_String (V : Version_Number) return String;
   function Patch_String (V : Version_Number) return String;

   -- Version constants
   Zero_Version : constant Version_Number := (0, 0, 0);
   Max_Version  : constant Version_Number := (255, 255, 255);

end GNAT.Version;
