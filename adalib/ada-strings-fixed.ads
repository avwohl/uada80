-- Ada.Strings.Fixed for Z80
-- String manipulation for fixed-length strings

with Ada.Strings;

package Ada.Strings.Fixed is
   pragma Preelaborate;

   -- Copy operations
   procedure Move
     (Source  : String;
      Target  : out String;
      Drop    : Truncation := Error;
      Justify : Alignment := Left;
      Pad     : Character := Space);

   -- Search functions
   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward) return Natural;

   function Index
     (Source : String;
      Set    : String;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural;

   function Index_Non_Blank
     (Source : String;
      Going  : Direction := Forward) return Natural;

   function Count
     (Source  : String;
      Pattern : String) return Natural;

   function Count
     (Source : String;
      Set    : String) return Natural;

   -- Transformation functions
   function Replace_Slice
     (Source : String;
      Low    : Positive;
      High   : Natural;
      By     : String) return String;

   procedure Replace_Slice
     (Source  : in Out String;
      Low     : Positive;
      High    : Natural;
      By      : String;
      Drop    : Truncation := Error;
      Justify : Alignment := Left;
      Pad     : Character := Space);

   function Insert
     (Source   : String;
      Before   : Positive;
      New_Item : String) return String;

   procedure Insert
     (Source   : in Out String;
      Before   : Positive;
      New_Item : String;
      Drop     : Truncation := Error);

   function Overwrite
     (Source   : String;
      Position : Positive;
      New_Item : String) return String;

   procedure Overwrite
     (Source   : in Out String;
      Position : Positive;
      New_Item : String;
      Drop     : Truncation := Right);

   function Delete
     (Source  : String;
      From    : Positive;
      Through : Natural) return String;

   procedure Delete
     (Source  : in Out String;
      From    : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space);

   -- Trim functions
   function Trim
     (Source : String;
      Side   : Trim_End) return String;

   procedure Trim
     (Source  : in Out String;
      Side    : Trim_End;
      Justify : Alignment := Left;
      Pad     : Character := Space);

   function Trim
     (Source : String;
      Left   : String;
      Right  : String) return String;

   procedure Trim
     (Source  : in Out String;
      Left    : String;
      Right   : String;
      Justify : Alignment := Strings.Left;
      Pad     : Character := Space);

   -- Head and Tail
   function Head
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String;

   procedure Head
     (Source  : in Out String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space);

   function Tail
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String;

   procedure Tail
     (Source  : in Out String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space);

   -- Replication
   function "*" (Left : Natural; Right : Character) return String;
   function "*" (Left : Natural; Right : String) return String;

end Ada.Strings.Fixed;
