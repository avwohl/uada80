-- Ada.Strings.Wide_Bounded for Z80
-- Bounded-length wide string handling
--
-- Provides generic bounded wide strings with compile-time maximum length.

with Ada.Strings;

generic
   Max : Positive;  -- Maximum string length
package Ada.Strings.Wide_Bounded is
   pragma Preelaborate;

   Max_Length : constant Positive := Max;

   type Bounded_Wide_String is private;

   Null_Bounded_Wide_String : constant Bounded_Wide_String;

   subtype Length_Range is Natural range 0 .. Max_Length;

   function Length (Source : Bounded_Wide_String) return Length_Range;

   -- Conversion functions
   function To_Bounded_Wide_String
     (Source : Wide_String;
      Drop   : Truncation := Error) return Bounded_Wide_String;

   function To_Wide_String (Source : Bounded_Wide_String) return Wide_String;

   procedure Set_Bounded_Wide_String
     (Target : out Bounded_Wide_String;
      Source : Wide_String;
      Drop   : Truncation := Error);

   -- Append operations
   procedure Append
     (Source   : in out Bounded_Wide_String;
      New_Item : Bounded_Wide_String;
      Drop     : Truncation := Error);

   procedure Append
     (Source   : in Out Bounded_Wide_String;
      New_Item : Wide_String;
      Drop     : Truncation := Error);

   procedure Append
     (Source   : in Out Bounded_Wide_String;
      New_Item : Wide_Character;
      Drop     : Truncation := Error);

   function "&"
     (Left, Right : Bounded_Wide_String) return Bounded_Wide_String;

   function "&"
     (Left : Bounded_Wide_String; Right : Wide_String) return Bounded_Wide_String;

   function "&"
     (Left : Wide_String; Right : Bounded_Wide_String) return Bounded_Wide_String;

   function "&"
     (Left : Bounded_Wide_String; Right : Wide_Character) return Bounded_Wide_String;

   function "&"
     (Left : Wide_Character; Right : Bounded_Wide_String) return Bounded_Wide_String;

   -- Element access
   function Element
     (Source : Bounded_Wide_String;
      Index  : Positive) return Wide_Character;

   procedure Replace_Element
     (Source : in Out Bounded_Wide_String;
      Index  : Positive;
      By     : Wide_Character);

   -- Slice operations
   function Slice
     (Source : Bounded_Wide_String;
      Low    : Positive;
      High   : Natural) return Wide_String;

   function Bounded_Slice
     (Source : Bounded_Wide_String;
      Low    : Positive;
      High   : Natural) return Bounded_Wide_String;

   procedure Bounded_Slice
     (Source : Bounded_Wide_String;
      Target : out Bounded_Wide_String;
      Low    : Positive;
      High   : Natural);

   -- Comparison functions
   function "="  (Left, Right : Bounded_Wide_String) return Boolean;
   function "="  (Left : Bounded_Wide_String; Right : Wide_String) return Boolean;
   function "="  (Left : Wide_String; Right : Bounded_Wide_String) return Boolean;

   function "<"  (Left, Right : Bounded_Wide_String) return Boolean;
   function "<"  (Left : Bounded_Wide_String; Right : Wide_String) return Boolean;
   function "<"  (Left : Wide_String; Right : Bounded_Wide_String) return Boolean;

   function "<=" (Left, Right : Bounded_Wide_String) return Boolean;
   function "<=" (Left : Bounded_Wide_String; Right : Wide_String) return Boolean;
   function "<=" (Left : Wide_String; Right : Bounded_Wide_String) return Boolean;

   function ">"  (Left, Right : Bounded_Wide_String) return Boolean;
   function ">"  (Left : Bounded_Wide_String; Right : Wide_String) return Boolean;
   function ">"  (Left : Wide_String; Right : Bounded_Wide_String) return Boolean;

   function ">=" (Left, Right : Bounded_Wide_String) return Boolean;
   function ">=" (Left : Bounded_Wide_String; Right : Wide_String) return Boolean;
   function ">=" (Left : Wide_String; Right : Bounded_Wide_String) return Boolean;

   -- Search functions
   function Index
     (Source  : Bounded_Wide_String;
      Pattern : Wide_String;
      Going   : Direction := Forward) return Natural;

   function Index_Non_Blank
     (Source : Bounded_Wide_String;
      Going  : Direction := Forward) return Natural;

   function Count
     (Source  : Bounded_Wide_String;
      Pattern : Wide_String) return Natural;

   -- Transformation functions
   function Replace_Slice
     (Source : Bounded_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_String;
      Drop   : Truncation := Error) return Bounded_Wide_String;

   procedure Replace_Slice
     (Source : in Out Bounded_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_String;
      Drop   : Truncation := Error);

   function Insert
     (Source   : Bounded_Wide_String;
      Before   : Positive;
      New_Item : Wide_String;
      Drop     : Truncation := Error) return Bounded_Wide_String;

   procedure Insert
     (Source   : in Out Bounded_Wide_String;
      Before   : Positive;
      New_Item : Wide_String;
      Drop     : Truncation := Error);

   function Overwrite
     (Source   : Bounded_Wide_String;
      Position : Positive;
      New_Item : Wide_String;
      Drop     : Truncation := Error) return Bounded_Wide_String;

   procedure Overwrite
     (Source   : in Out Bounded_Wide_String;
      Position : Positive;
      New_Item : Wide_String;
      Drop     : Truncation := Error);

   function Delete
     (Source  : Bounded_Wide_String;
      From    : Positive;
      Through : Natural) return Bounded_Wide_String;

   procedure Delete
     (Source  : in Out Bounded_Wide_String;
      From    : Positive;
      Through : Natural);

   -- Trim functions
   function Trim
     (Source : Bounded_Wide_String;
      Side   : Trim_End) return Bounded_Wide_String;

   procedure Trim
     (Source : in Out Bounded_Wide_String;
      Side   : Trim_End);

   -- Head and Tail
   function Head
     (Source : Bounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space;
      Drop   : Truncation := Error) return Bounded_Wide_String;

   procedure Head
     (Source : in Out Bounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space;
      Drop   : Truncation := Error);

   function Tail
     (Source : Bounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space;
      Drop   : Truncation := Error) return Bounded_Wide_String;

   procedure Tail
     (Source : in Out Bounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space;
      Drop   : Truncation := Error);

   -- Replication
   function "*"
     (Left  : Natural;
      Right : Wide_Character) return Bounded_Wide_String;

   function "*"
     (Left  : Natural;
      Right : Wide_String) return Bounded_Wide_String;

   function "*"
     (Left  : Natural;
      Right : Bounded_Wide_String) return Bounded_Wide_String;

private

   type Bounded_Wide_String is record
      Data   : Wide_String (1 .. Max_Length);
      Length : Length_Range := 0;
   end record;

   Null_Bounded_Wide_String : constant Bounded_Wide_String :=
     (Data => (others => Wide_Character'Val (32)), Length => 0);

end Ada.Strings.Wide_Bounded;
