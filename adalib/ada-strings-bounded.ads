-- Ada.Strings.Bounded for Z80
-- Bounded-length string handling
--
-- Provides generic bounded strings with compile-time maximum length.

with Ada.Strings;

generic
   Max : Positive;  -- Maximum string length
package Ada.Strings.Bounded is
   pragma Preelaborate;

   Max_Length : constant Positive := Max;

   type Bounded_String is private;

   Null_Bounded_String : constant Bounded_String;

   subtype Length_Range is Natural range 0 .. Max_Length;

   function Length (Source : Bounded_String) return Length_Range;

   -- Conversion functions
   function To_Bounded_String
     (Source : String;
      Drop   : Truncation := Error) return Bounded_String;

   function To_String (Source : Bounded_String) return String;

   procedure Set_Bounded_String
     (Target : out Bounded_String;
      Source : String;
      Drop   : Truncation := Error);

   -- Append operations
   procedure Append
     (Source   : in Out Bounded_String;
      New_Item : Bounded_String;
      Drop     : Truncation := Error);

   procedure Append
     (Source   : in Out Bounded_String;
      New_Item : String;
      Drop     : Truncation := Error);

   procedure Append
     (Source   : in Out Bounded_String;
      New_Item : Character;
      Drop     : Truncation := Error);

   function "&"
     (Left, Right : Bounded_String) return Bounded_String;

   function "&"
     (Left : Bounded_String; Right : String) return Bounded_String;

   function "&"
     (Left : String; Right : Bounded_String) return Bounded_String;

   function "&"
     (Left : Bounded_String; Right : Character) return Bounded_String;

   function "&"
     (Left : Character; Right : Bounded_String) return Bounded_String;

   -- Element access
   function Element
     (Source : Bounded_String;
      Index  : Positive) return Character;

   procedure Replace_Element
     (Source : in Out Bounded_String;
      Index  : Positive;
      By     : Character);

   -- Slice operations
   function Slice
     (Source : Bounded_String;
      Low    : Positive;
      High   : Natural) return String;

   function Bounded_Slice
     (Source : Bounded_String;
      Low    : Positive;
      High   : Natural) return Bounded_String;

   procedure Bounded_Slice
     (Source : Bounded_String;
      Target : out Bounded_String;
      Low    : Positive;
      High   : Natural);

   -- Comparison functions
   function "="  (Left, Right : Bounded_String) return Boolean;
   function "="  (Left : Bounded_String; Right : String) return Boolean;
   function "="  (Left : String; Right : Bounded_String) return Boolean;

   function "<"  (Left, Right : Bounded_String) return Boolean;
   function "<"  (Left : Bounded_String; Right : String) return Boolean;
   function "<"  (Left : String; Right : Bounded_String) return Boolean;

   function "<=" (Left, Right : Bounded_String) return Boolean;
   function "<=" (Left : Bounded_String; Right : String) return Boolean;
   function "<=" (Left : String; Right : Bounded_String) return Boolean;

   function ">"  (Left, Right : Bounded_String) return Boolean;
   function ">"  (Left : Bounded_String; Right : String) return Boolean;
   function ">"  (Left : String; Right : Bounded_String) return Boolean;

   function ">=" (Left, Right : Bounded_String) return Boolean;
   function ">=" (Left : Bounded_String; Right : String) return Boolean;
   function ">=" (Left : String; Right : Bounded_String) return Boolean;

   -- Search functions
   function Index
     (Source  : Bounded_String;
      Pattern : String;
      Going   : Direction := Forward) return Natural;

   function Index
     (Source : Bounded_String;
      Set    : String;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural;

   function Index_Non_Blank
     (Source : Bounded_String;
      Going  : Direction := Forward) return Natural;

   function Count
     (Source  : Bounded_String;
      Pattern : String) return Natural;

   function Count
     (Source : Bounded_String;
      Set    : String) return Natural;

   -- Transformation functions
   function Replace_Slice
     (Source : Bounded_String;
      Low    : Positive;
      High   : Natural;
      By     : String;
      Drop   : Truncation := Error) return Bounded_String;

   procedure Replace_Slice
     (Source : in Out Bounded_String;
      Low    : Positive;
      High   : Natural;
      By     : String;
      Drop   : Truncation := Error);

   function Insert
     (Source   : Bounded_String;
      Before   : Positive;
      New_Item : String;
      Drop     : Truncation := Error) return Bounded_String;

   procedure Insert
     (Source   : in Out Bounded_String;
      Before   : Positive;
      New_Item : String;
      Drop     : Truncation := Error);

   function Overwrite
     (Source   : Bounded_String;
      Position : Positive;
      New_Item : String;
      Drop     : Truncation := Error) return Bounded_String;

   procedure Overwrite
     (Source   : in Out Bounded_String;
      Position : Positive;
      New_Item : String;
      Drop     : Truncation := Error);

   function Delete
     (Source  : Bounded_String;
      From    : Positive;
      Through : Natural) return Bounded_String;

   procedure Delete
     (Source  : in Out Bounded_String;
      From    : Positive;
      Through : Natural);

   -- Trim functions
   function Trim
     (Source : Bounded_String;
      Side   : Trim_End) return Bounded_String;

   procedure Trim
     (Source : in Out Bounded_String;
      Side   : Trim_End);

   function Trim
     (Source : Bounded_String;
      Left   : String;
      Right  : String) return Bounded_String;

   procedure Trim
     (Source : in Out Bounded_String;
      Left   : String;
      Right  : String);

   -- Head and Tail
   function Head
     (Source : Bounded_String;
      Count  : Natural;
      Pad    : Character := Space;
      Drop   : Truncation := Error) return Bounded_String;

   procedure Head
     (Source : in Out Bounded_String;
      Count  : Natural;
      Pad    : Character := Space;
      Drop   : Truncation := Error);

   function Tail
     (Source : Bounded_String;
      Count  : Natural;
      Pad    : Character := Space;
      Drop   : Truncation := Error) return Bounded_String;

   procedure Tail
     (Source : in Out Bounded_String;
      Count  : Natural;
      Pad    : Character := Space;
      Drop   : Truncation := Error);

   -- Replication
   function "*"
     (Left  : Natural;
      Right : Character) return Bounded_String;

   function "*"
     (Left  : Natural;
      Right : String) return Bounded_String;

   function "*"
     (Left  : Natural;
      Right : Bounded_String) return Bounded_String;

private

   type Bounded_String is record
      Data   : String (1 .. Max_Length);
      Length : Length_Range := 0;
   end record;

   Null_Bounded_String : constant Bounded_String :=
     (Data => (others => ' '), Length => 0);

end Ada.Strings.Bounded;
