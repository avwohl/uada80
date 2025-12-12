-- Ada.Strings.Unbounded for Z80
-- Unbounded-length string handling
--
-- Note: On Z80 with limited memory, "unbounded" strings have practical limits.
-- This implementation uses a maximum internal buffer.

with Ada.Strings;

package Ada.Strings.Unbounded is
   pragma Preelaborate;

   -- Maximum string length for Z80 implementation
   Max_Length : constant := 255;

   type Unbounded_String is private;

   Null_Unbounded_String : constant Unbounded_String;

   function Length (Source : Unbounded_String) return Natural;

   type String_Access is access all String;

   -- Conversion functions
   function To_Unbounded_String (Source : String) return Unbounded_String;
   function To_Unbounded_String (Length : Natural) return Unbounded_String;
   function To_String (Source : Unbounded_String) return String;

   procedure Set_Unbounded_String
     (Target : out Unbounded_String;
      Source : String);

   -- Append operations
   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : Unbounded_String);

   procedure Append
     (Source   : in Out Unbounded_String;
      New_Item : String);

   procedure Append
     (Source   : in Out Unbounded_String;
      New_Item : Character);

   function "&" (Left, Right : Unbounded_String) return Unbounded_String;
   function "&" (Left : Unbounded_String; Right : String) return Unbounded_String;
   function "&" (Left : String; Right : Unbounded_String) return Unbounded_String;
   function "&" (Left : Unbounded_String; Right : Character) return Unbounded_String;
   function "&" (Left : Character; Right : Unbounded_String) return Unbounded_String;

   -- Element access
   function Element
     (Source : Unbounded_String;
      Index  : Positive) return Character;

   procedure Replace_Element
     (Source : in Out Unbounded_String;
      Index  : Positive;
      By     : Character);

   -- Slice operations
   function Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural) return String;

   function Unbounded_Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural) return Unbounded_String;

   procedure Unbounded_Slice
     (Source : Unbounded_String;
      Target : out Unbounded_String;
      Low    : Positive;
      High   : Natural);

   -- Comparison functions
   function "="  (Left, Right : Unbounded_String) return Boolean;
   function "="  (Left : Unbounded_String; Right : String) return Boolean;
   function "="  (Left : String; Right : Unbounded_String) return Boolean;

   function "<"  (Left, Right : Unbounded_String) return Boolean;
   function "<"  (Left : Unbounded_String; Right : String) return Boolean;
   function "<"  (Left : String; Right : Unbounded_String) return Boolean;

   function "<=" (Left, Right : Unbounded_String) return Boolean;
   function "<=" (Left : Unbounded_String; Right : String) return Boolean;
   function "<=" (Left : String; Right : Unbounded_String) return Boolean;

   function ">"  (Left, Right : Unbounded_String) return Boolean;
   function ">"  (Left : Unbounded_String; Right : String) return Boolean;
   function ">"  (Left : String; Right : Unbounded_String) return Boolean;

   function ">=" (Left, Right : Unbounded_String) return Boolean;
   function ">=" (Left : Unbounded_String; Right : String) return Boolean;
   function ">=" (Left : String; Right : Unbounded_String) return Boolean;

   -- Search functions (using patterns from Ada.Strings.Fixed)
   function Index
     (Source  : Unbounded_String;
      Pattern : String;
      Going   : Direction := Forward) return Natural;

   function Index
     (Source : Unbounded_String;
      Set    : String;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural;

   function Index_Non_Blank
     (Source : Unbounded_String;
      Going  : Direction := Forward) return Natural;

   function Count
     (Source  : Unbounded_String;
      Pattern : String) return Natural;

   function Count
     (Source : Unbounded_String;
      Set    : String) return Natural;

   -- Transformation functions
   function Replace_Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural;
      By     : String) return Unbounded_String;

   procedure Replace_Slice
     (Source : in Out Unbounded_String;
      Low    : Positive;
      High   : Natural;
      By     : String);

   function Insert
     (Source   : Unbounded_String;
      Before   : Positive;
      New_Item : String) return Unbounded_String;

   procedure Insert
     (Source   : in Out Unbounded_String;
      Before   : Positive;
      New_Item : String);

   function Overwrite
     (Source   : Unbounded_String;
      Position : Positive;
      New_Item : String) return Unbounded_String;

   procedure Overwrite
     (Source   : in Out Unbounded_String;
      Position : Positive;
      New_Item : String);

   function Delete
     (Source  : Unbounded_String;
      From    : Positive;
      Through : Natural) return Unbounded_String;

   procedure Delete
     (Source  : in Out Unbounded_String;
      From    : Positive;
      Through : Natural);

   -- Trim functions
   function Trim
     (Source : Unbounded_String;
      Side   : Trim_End) return Unbounded_String;

   procedure Trim
     (Source : in Out Unbounded_String;
      Side   : Trim_End);

   function Trim
     (Source : Unbounded_String;
      Left   : String;
      Right  : String) return Unbounded_String;

   procedure Trim
     (Source : in Out Unbounded_String;
      Left   : String;
      Right  : String);

   -- Head and Tail
   function Head
     (Source : Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space) return Unbounded_String;

   procedure Head
     (Source : in Out Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space);

   function Tail
     (Source : Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space) return Unbounded_String;

   procedure Tail
     (Source : in Out Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space);

   -- Replication
   function "*" (Left : Natural; Right : Character) return Unbounded_String;
   function "*" (Left : Natural; Right : String) return Unbounded_String;
   function "*" (Left : Natural; Right : Unbounded_String) return Unbounded_String;

private

   type Unbounded_String is record
      Data   : String (1 .. Max_Length);
      Length : Natural := 0;
   end record;

   Null_Unbounded_String : constant Unbounded_String :=
     (Data => (others => ' '), Length => 0);

end Ada.Strings.Unbounded;
