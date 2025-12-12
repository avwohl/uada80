-- Ada.Strings.Wide_Wide_Unbounded for Z80
-- Unbounded Wide_Wide_String handling

with Ada.Strings.Wide_Wide_Maps;
with Ada.Finalization;

package Ada.Strings.Wide_Wide_Unbounded is
   pragma Preelaborate;

   -- Maximum length for Z80 memory constraints
   Max_Length : constant := 255;

   type Unbounded_Wide_Wide_String is private;
   pragma Preelaborable_Initialization (Unbounded_Wide_Wide_String);

   Null_Unbounded_Wide_Wide_String : constant Unbounded_Wide_Wide_String;

   function Length (Source : Unbounded_Wide_Wide_String) return Natural;

   type Wide_Wide_String_Access is access all Wide_Wide_String;

   procedure Free (X : in Out Wide_Wide_String_Access);

   -- Conversion, Concatenation, and Selection Functions

   function To_Unbounded_Wide_Wide_String
     (Source : Wide_Wide_String) return Unbounded_Wide_Wide_String;

   function To_Unbounded_Wide_Wide_String
     (Length : Natural) return Unbounded_Wide_Wide_String;

   function To_Wide_Wide_String
     (Source : Unbounded_Wide_Wide_String) return Wide_Wide_String;

   procedure Set_Unbounded_Wide_Wide_String
     (Target : out Unbounded_Wide_Wide_String;
      Source : Wide_Wide_String);

   procedure Append
     (Source   : in Out Unbounded_Wide_Wide_String;
      New_Item : Unbounded_Wide_Wide_String);

   procedure Append
     (Source   : in Out Unbounded_Wide_Wide_String;
      New_Item : Wide_Wide_String);

   procedure Append
     (Source   : in Out Unbounded_Wide_Wide_String;
      New_Item : Wide_Wide_Character);

   function "&"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String;

   function "&"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Unbounded_Wide_Wide_String;

   function "&"
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String;

   function "&"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_Character) return Unbounded_Wide_Wide_String;

   function "&"
     (Left  : Wide_Wide_Character;
      Right : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String;

   function Element
     (Source : Unbounded_Wide_Wide_String;
      Index  : Positive) return Wide_Wide_Character;

   procedure Replace_Element
     (Source : in Out Unbounded_Wide_Wide_String;
      Index  : Positive;
      By     : Wide_Wide_Character);

   function Slice
     (Source : Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural) return Wide_Wide_String;

   function Unbounded_Slice
     (Source : Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural) return Unbounded_Wide_Wide_String;

   procedure Unbounded_Slice
     (Source : Unbounded_Wide_Wide_String;
      Target : out Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural);

   function "="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function "="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean;

   function "="
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function "<"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function "<"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean;

   function "<"
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function "<="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function "<="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean;

   function "<="
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function ">"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function ">"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean;

   function ">"
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function ">="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function ">="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean;

   function ">="
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   -- Search Functions

   function Index
     (Source  : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity) return Natural;

   function Index
     (Source : Unbounded_Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural;

   function Index_Non_Blank
     (Source : Unbounded_Wide_Wide_String;
      Going  : Direction := Forward) return Natural;

   function Count
     (Source  : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity) return Natural;

   function Count
     (Source : Unbounded_Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set) return Natural;

   -- Transformation Functions

   function Translate
     (Source  : Unbounded_Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
     return Unbounded_Wide_Wide_String;

   procedure Translate
     (Source  : in Out Unbounded_Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping);

   function Replace_Slice
     (Source : Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_Wide_String) return Unbounded_Wide_Wide_String;

   procedure Replace_Slice
     (Source : in Out Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_Wide_String);

   function Insert
     (Source   : Unbounded_Wide_Wide_String;
      Before   : Positive;
      New_Item : Wide_Wide_String) return Unbounded_Wide_Wide_String;

   procedure Insert
     (Source   : in Out Unbounded_Wide_Wide_String;
      Before   : Positive;
      New_Item : Wide_Wide_String);

   function Overwrite
     (Source   : Unbounded_Wide_Wide_String;
      Position : Positive;
      New_Item : Wide_Wide_String) return Unbounded_Wide_Wide_String;

   procedure Overwrite
     (Source   : in Out Unbounded_Wide_Wide_String;
      Position : Positive;
      New_Item : Wide_Wide_String);

   function Delete
     (Source  : Unbounded_Wide_Wide_String;
      From    : Positive;
      Through : Natural) return Unbounded_Wide_Wide_String;

   procedure Delete
     (Source  : in Out Unbounded_Wide_Wide_String;
      From    : Positive;
      Through : Natural);

   function Trim
     (Source : Unbounded_Wide_Wide_String;
      Side   : Trim_End) return Unbounded_Wide_Wide_String;

   procedure Trim
     (Source : in Out Unbounded_Wide_Wide_String;
      Side   : Trim_End);

   function Trim
     (Source : Unbounded_Wide_Wide_String;
      Left   : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right  : Wide_Wide_Maps.Wide_Wide_Character_Set)
     return Unbounded_Wide_Wide_String;

   procedure Trim
     (Source : in Out Unbounded_Wide_Wide_String;
      Left   : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right  : Wide_Wide_Maps.Wide_Wide_Character_Set);

   function Head
     (Source : Unbounded_Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Character'Val (32))
     return Unbounded_Wide_Wide_String;

   procedure Head
     (Source : in Out Unbounded_Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Character'Val (32));

   function Tail
     (Source : Unbounded_Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Character'Val (32))
     return Unbounded_Wide_Wide_String;

   procedure Tail
     (Source : in Out Unbounded_Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Character'Val (32));

   function "*"
     (Left  : Natural;
      Right : Wide_Wide_Character) return Unbounded_Wide_Wide_String;

   function "*"
     (Left  : Natural;
      Right : Wide_Wide_String) return Unbounded_Wide_Wide_String;

   function "*"
     (Left  : Natural;
      Right : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String;

private

   type String_Data is array (1 .. Max_Length) of Wide_Wide_Character;

   type Unbounded_Wide_Wide_String is record
      Data   : String_Data := (others => Wide_Wide_Character'Val (0));
      Length : Natural := 0;
   end record;

   Null_Unbounded_Wide_Wide_String : constant Unbounded_Wide_Wide_String :=
     (Data => (others => Wide_Wide_Character'Val (0)), Length => 0);

end Ada.Strings.Wide_Wide_Unbounded;
