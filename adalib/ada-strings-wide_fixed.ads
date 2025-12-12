-- Ada.Strings.Wide_Fixed for Z80
-- Fixed-length wide string handling

package Ada.Strings.Wide_Fixed is
   pragma Preelaborate;

   -- Move procedure
   procedure Move
     (Source  : Wide_String;
      Target  : out Wide_String;
      Drop    : Truncation := Error;
      Justify : Alignment := Left;
      Pad     : Wide_Character := Wide_Character'Val (32));

   -- Search functions
   function Index
     (Source  : Wide_String;
      Pattern : Wide_String;
      Going   : Direction := Forward) return Natural;

   function Index
     (Source : Wide_String;
      Set    : Wide_String;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural;

   function Index_Non_Blank
     (Source : Wide_String;
      Going  : Direction := Forward) return Natural;

   function Count
     (Source  : Wide_String;
      Pattern : Wide_String) return Natural;

   function Count
     (Source : Wide_String;
      Set    : Wide_String) return Natural;

   -- String transformation
   function Replace_Slice
     (Source : Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_String) return Wide_String;

   procedure Replace_Slice
     (Source : in Out Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_String;
      Drop   : Truncation := Error;
      Justify : Alignment := Left;
      Pad    : Wide_Character := Wide_Character'Val (32));

   function Insert
     (Source   : Wide_String;
      Before   : Positive;
      New_Item : Wide_String) return Wide_String;

   procedure Insert
     (Source   : in Out Wide_String;
      Before   : Positive;
      New_Item : Wide_String;
      Drop     : Truncation := Error);

   function Overwrite
     (Source   : Wide_String;
      Position : Positive;
      New_Item : Wide_String) return Wide_String;

   procedure Overwrite
     (Source   : in Out Wide_String;
      Position : Positive;
      New_Item : Wide_String;
      Drop     : Truncation := Right);

   function Delete
     (Source  : Wide_String;
      From    : Positive;
      Through : Natural) return Wide_String;

   procedure Delete
     (Source  : in Out Wide_String;
      From    : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad     : Wide_Character := Wide_Character'Val (32));

   -- Trim functions
   function Trim
     (Source : Wide_String;
      Side   : Trim_End) return Wide_String;

   procedure Trim
     (Source  : in Out Wide_String;
      Side    : Trim_End;
      Justify : Alignment := Left;
      Pad     : Wide_Character := Wide_Character'Val (32));

   function Trim
     (Source : Wide_String;
      Left   : Wide_String;
      Right  : Wide_String) return Wide_String;

   procedure Trim
     (Source  : in Out Wide_String;
      Left    : Wide_String;
      Right   : Wide_String;
      Justify : Alignment := Ada.Strings.Left;
      Pad     : Wide_Character := Wide_Character'Val (32));

   -- Head and Tail
   function Head
     (Source : Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Character'Val (32)) return Wide_String;

   procedure Head
     (Source  : in Out Wide_String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Wide_Character := Wide_Character'Val (32));

   function Tail
     (Source : Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Character'Val (32)) return Wide_String;

   procedure Tail
     (Source  : in Out Wide_String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Wide_Character := Wide_Character'Val (32));

   -- String multiplication
   function "*" (Left : Natural; Right : Wide_Character) return Wide_String;
   function "*" (Left : Natural; Right : Wide_String) return Wide_String;

end Ada.Strings.Wide_Fixed;
