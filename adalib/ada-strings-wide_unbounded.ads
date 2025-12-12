-- Ada.Strings.Wide_Unbounded for Z80
-- Unbounded-length wide string handling
--
-- Note: On Z80 with limited memory, "unbounded" wide strings have practical limits.

with Ada.Strings;

package Ada.Strings.Wide_Unbounded is
   pragma Preelaborate;

   -- Maximum wide string length for Z80 implementation
   Max_Length : constant := 127;

   type Unbounded_Wide_String is private;

   Null_Unbounded_Wide_String : constant Unbounded_Wide_String;

   function Length (Source : Unbounded_Wide_String) return Natural;

   -- Conversion functions
   function To_Unbounded_Wide_String (Source : Wide_String) return Unbounded_Wide_String;
   function To_Unbounded_Wide_String (Length : Natural) return Unbounded_Wide_String;
   function To_Wide_String (Source : Unbounded_Wide_String) return Wide_String;

   procedure Set_Unbounded_Wide_String
     (Target : out Unbounded_Wide_String;
      Source : Wide_String);

   -- Append operations
   procedure Append
     (Source   : in Out Unbounded_Wide_String;
      New_Item : Unbounded_Wide_String);

   procedure Append
     (Source   : in Out Unbounded_Wide_String;
      New_Item : Wide_String);

   procedure Append
     (Source   : in Out Unbounded_Wide_String;
      New_Item : Wide_Character);

   function "&" (Left, Right : Unbounded_Wide_String) return Unbounded_Wide_String;
   function "&" (Left : Unbounded_Wide_String; Right : Wide_String) return Unbounded_Wide_String;
   function "&" (Left : Wide_String; Right : Unbounded_Wide_String) return Unbounded_Wide_String;
   function "&" (Left : Unbounded_Wide_String; Right : Wide_Character) return Unbounded_Wide_String;
   function "&" (Left : Wide_Character; Right : Unbounded_Wide_String) return Unbounded_Wide_String;

   -- Element access
   function Element
     (Source : Unbounded_Wide_String;
      Index  : Positive) return Wide_Character;

   procedure Replace_Element
     (Source : in Out Unbounded_Wide_String;
      Index  : Positive;
      By     : Wide_Character);

   -- Slice operations
   function Slice
     (Source : Unbounded_Wide_String;
      Low    : Positive;
      High   : Natural) return Wide_String;

   function Unbounded_Slice
     (Source : Unbounded_Wide_String;
      Low    : Positive;
      High   : Natural) return Unbounded_Wide_String;

   -- Comparison functions
   function "="  (Left, Right : Unbounded_Wide_String) return Boolean;
   function "="  (Left : Unbounded_Wide_String; Right : Wide_String) return Boolean;
   function "="  (Left : Wide_String; Right : Unbounded_Wide_String) return Boolean;

   function "<"  (Left, Right : Unbounded_Wide_String) return Boolean;
   function "<"  (Left : Unbounded_Wide_String; Right : Wide_String) return Boolean;
   function "<"  (Left : Wide_String; Right : Unbounded_Wide_String) return Boolean;

   function "<=" (Left, Right : Unbounded_Wide_String) return Boolean;
   function ">"  (Left, Right : Unbounded_Wide_String) return Boolean;
   function ">=" (Left, Right : Unbounded_Wide_String) return Boolean;

   -- Search functions
   function Index
     (Source  : Unbounded_Wide_String;
      Pattern : Wide_String;
      Going   : Direction := Forward) return Natural;

   function Index_Non_Blank
     (Source : Unbounded_Wide_String;
      Going  : Direction := Forward) return Natural;

   function Count
     (Source  : Unbounded_Wide_String;
      Pattern : Wide_String) return Natural;

   -- Trim functions
   function Trim
     (Source : Unbounded_Wide_String;
      Side   : Trim_End) return Unbounded_Wide_String;

   procedure Trim
     (Source : in Out Unbounded_Wide_String;
      Side   : Trim_End);

   -- Head and Tail
   function Head
     (Source : Unbounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Character'Val (32)) return Unbounded_Wide_String;

   function Tail
     (Source : Unbounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Character'Val (32)) return Unbounded_Wide_String;

   -- Replication
   function "*" (Left : Natural; Right : Wide_Character) return Unbounded_Wide_String;
   function "*" (Left : Natural; Right : Wide_String) return Unbounded_Wide_String;
   function "*" (Left : Natural; Right : Unbounded_Wide_String) return Unbounded_Wide_String;

private

   type Unbounded_Wide_String is record
      Data   : Wide_String (1 .. Max_Length);
      Length : Natural := 0;
   end record;

   Null_Unbounded_Wide_String : constant Unbounded_Wide_String :=
     (Data => (others => Wide_Character'Val (32)), Length => 0);

end Ada.Strings.Wide_Unbounded;
