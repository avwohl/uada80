-- Ada.Strings.Wide_Wide_Fixed for Z80
-- Wide_Wide_String fixed-length string handling

with Ada.Strings.Wide_Wide_Maps;

package Ada.Strings.Wide_Wide_Fixed is
   pragma Preelaborate;

   -- "Copy" procedure for strings of possibly different lengths

   procedure Move
     (Source  : Wide_Wide_String;
      Target  : out Wide_Wide_String;
      Drop    : Truncation := Error;
      Justify : Alignment := Left;
      Pad     : Wide_Wide_Character := Wide_Wide_Space);

   -- Search subprograms

   function Index
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity) return Natural;

   function Index
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function) return Natural;

   function Index
     (Source : Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural;

   function Index
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity) return Natural;

   function Index_Non_Blank
     (Source : Wide_Wide_String;
      Going  : Direction := Forward) return Natural;

   function Count
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity) return Natural;

   function Count
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function) return Natural;

   function Count
     (Source : Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set) return Natural;

   -- String transformation subprograms

   function Replace_Slice
     (Source : Wide_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_Wide_String) return Wide_Wide_String;

   procedure Replace_Slice
     (Source  : in Out Wide_Wide_String;
      Low     : Positive;
      High    : Natural;
      By      : Wide_Wide_String;
      Drop    : Truncation := Error;
      Justify : Alignment := Left;
      Pad     : Wide_Wide_Character := Wide_Wide_Space);

   function Insert
     (Source   : Wide_Wide_String;
      Before   : Positive;
      New_Item : Wide_Wide_String) return Wide_Wide_String;

   procedure Insert
     (Source   : in Out Wide_Wide_String;
      Before   : Positive;
      New_Item : Wide_Wide_String;
      Drop     : Truncation := Error);

   function Overwrite
     (Source   : Wide_Wide_String;
      Position : Positive;
      New_Item : Wide_Wide_String) return Wide_Wide_String;

   procedure Overwrite
     (Source   : in Out Wide_Wide_String;
      Position : Positive;
      New_Item : Wide_Wide_String;
      Drop     : Truncation := Right);

   function Delete
     (Source  : Wide_Wide_String;
      From    : Positive;
      Through : Natural) return Wide_Wide_String;

   procedure Delete
     (Source  : in Out Wide_Wide_String;
      From    : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad     : Wide_Wide_Character := Wide_Wide_Space);

   -- String selector subprograms

   function Trim
     (Source : Wide_Wide_String;
      Side   : Trim_End) return Wide_Wide_String;

   procedure Trim
     (Source  : in Out Wide_Wide_String;
      Side    : Trim_End;
      Justify : Alignment := Left;
      Pad     : Wide_Wide_Character := Wide_Wide_Space);

   function Trim
     (Source : Wide_Wide_String;
      Left   : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right  : Wide_Wide_Maps.Wide_Wide_Character_Set) return Wide_Wide_String;

   procedure Trim
     (Source  : in Out Wide_Wide_String;
      Left    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right   : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Justify : Alignment := Strings.Left;
      Pad     : Wide_Wide_Character := Wide_Wide_Space);

   function Head
     (Source : Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Space) return Wide_Wide_String;

   procedure Head
     (Source  : in Out Wide_Wide_String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Wide_Wide_Character := Wide_Wide_Space);

   function Tail
     (Source : Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Space) return Wide_Wide_String;

   procedure Tail
     (Source  : in Out Wide_Wide_String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Wide_Wide_Character := Wide_Wide_Space);

   -- String constructor subprograms

   function "*"
     (Left  : Natural;
      Right : Wide_Wide_Character) return Wide_Wide_String;

   function "*"
     (Left  : Natural;
      Right : Wide_Wide_String) return Wide_Wide_String;

   -- Character mapping subprograms

   function Translate
     (Source  : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping) return Wide_Wide_String;

   procedure Translate
     (Source  : in Out Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping);

   function Translate
     (Source  : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function) return Wide_Wide_String;

   procedure Translate
     (Source  : in Out Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function);

private

   Wide_Wide_Space : constant Wide_Wide_Character := Wide_Wide_Character'Val (32);

end Ada.Strings.Wide_Wide_Fixed;
