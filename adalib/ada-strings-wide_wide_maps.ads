-- Ada.Strings.Wide_Wide_Maps for Z80
-- Wide_Wide_Character mapping support

package Ada.Strings.Wide_Wide_Maps is
   pragma Preelaborate;

   -- Maximum set size for Z80
   Max_Set_Size : constant := 64;

   type Wide_Wide_Character_Set is private;
   pragma Preelaborable_Initialization (Wide_Wide_Character_Set);

   Null_Set : constant Wide_Wide_Character_Set;

   type Wide_Wide_Character_Range is record
      Low  : Wide_Wide_Character;
      High : Wide_Wide_Character;
   end record;

   type Wide_Wide_Character_Ranges is array (Positive range <>) of Wide_Wide_Character_Range;

   type Wide_Wide_Character_Mapping is private;
   pragma Preelaborable_Initialization (Wide_Wide_Character_Mapping);

   Identity : constant Wide_Wide_Character_Mapping;

   type Wide_Wide_Character_Mapping_Function is
     access function (From : Wide_Wide_Character) return Wide_Wide_Character;

   -- Set operations
   function To_Set (Ranges : Wide_Wide_Character_Ranges) return Wide_Wide_Character_Set;
   function To_Set (Span : Wide_Wide_Character_Range) return Wide_Wide_Character_Set;
   function To_Ranges (Set : Wide_Wide_Character_Set) return Wide_Wide_Character_Ranges;

   function "=" (Left, Right : Wide_Wide_Character_Set) return Boolean;

   function "not" (Right : Wide_Wide_Character_Set) return Wide_Wide_Character_Set;
   function "and" (Left, Right : Wide_Wide_Character_Set) return Wide_Wide_Character_Set;
   function "or" (Left, Right : Wide_Wide_Character_Set) return Wide_Wide_Character_Set;
   function "xor" (Left, Right : Wide_Wide_Character_Set) return Wide_Wide_Character_Set;
   function "-" (Left, Right : Wide_Wide_Character_Set) return Wide_Wide_Character_Set;

   function Is_In
     (Element : Wide_Wide_Character;
      Set     : Wide_Wide_Character_Set) return Boolean;

   function Is_Subset
     (Elements : Wide_Wide_Character_Set;
      Set      : Wide_Wide_Character_Set) return Boolean;

   function "<="
     (Left  : Wide_Wide_Character_Set;
      Right : Wide_Wide_Character_Set) return Boolean renames Is_Subset;

   -- Mapping operations
   function To_Mapping
     (From, To : Wide_Wide_String) return Wide_Wide_Character_Mapping;

   function To_Domain
     (Map : Wide_Wide_Character_Mapping) return Wide_Wide_String;

   function To_Range
     (Map : Wide_Wide_Character_Mapping) return Wide_Wide_String;

   function Value
     (Map     : Wide_Wide_Character_Mapping;
      Element : Wide_Wide_Character) return Wide_Wide_Character;

private

   type Set_Array is array (1 .. Max_Set_Size) of Wide_Wide_Character;

   type Wide_Wide_Character_Set is record
      Chars  : Set_Array := (others => Wide_Wide_Character'Val (0));
      Length : Natural := 0;
   end record;

   Null_Set : constant Wide_Wide_Character_Set :=
     (Chars => (others => Wide_Wide_Character'Val (0)), Length => 0);

   type Mapping_Entry is record
      From : Wide_Wide_Character;
      To   : Wide_Wide_Character;
   end record;

   type Mapping_Array is array (1 .. Max_Set_Size) of Mapping_Entry;

   type Wide_Wide_Character_Mapping is record
      Entries : Mapping_Array := (others => (From => Wide_Wide_Character'Val (0),
                                             To => Wide_Wide_Character'Val (0)));
      Length  : Natural := 0;
   end record;

   Identity : constant Wide_Wide_Character_Mapping :=
     (Entries => (others => (From => Wide_Wide_Character'Val (0),
                             To => Wide_Wide_Character'Val (0))),
      Length => 0);

end Ada.Strings.Wide_Wide_Maps;
