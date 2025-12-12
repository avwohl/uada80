-- Ada.Strings.Wide_Maps for Z80
-- Wide character mapping and sets
--
-- Provides character sets and mappings for wide characters (limited to Latin-1)

package Ada.Strings.Wide_Maps is
   pragma Preelaborate;

   -- Maximum set size for Z80
   Max_Set_Size : constant := 64;

   ----------------------------
   -- Wide_Character_Set type --
   ----------------------------

   type Wide_Character_Set is private;
   pragma Preelaborable_Initialization (Wide_Character_Set);

   Null_Set : constant Wide_Character_Set;

   type Wide_Character_Range is record
      Low  : Wide_Character;
      High : Wide_Character;
   end record;

   type Wide_Character_Ranges is array (Positive range <>) of Wide_Character_Range;

   function To_Set (Ranges : Wide_Character_Ranges) return Wide_Character_Set;
   function To_Set (Span : Wide_Character_Range) return Wide_Character_Set;
   function To_Ranges (Set : Wide_Character_Set) return Wide_Character_Ranges;

   function "=" (Left, Right : Wide_Character_Set) return Boolean;
   function "not" (Right : Wide_Character_Set) return Wide_Character_Set;
   function "and" (Left, Right : Wide_Character_Set) return Wide_Character_Set;
   function "or" (Left, Right : Wide_Character_Set) return Wide_Character_Set;
   function "xor" (Left, Right : Wide_Character_Set) return Wide_Character_Set;
   function "-" (Left, Right : Wide_Character_Set) return Wide_Character_Set;

   function Is_In
     (Element : Wide_Character;
      Set     : Wide_Character_Set) return Boolean;

   function Is_Subset
     (Elements : Wide_Character_Set;
      Set      : Wide_Character_Set) return Boolean;

   function "<="
     (Left  : Wide_Character_Set;
      Right : Wide_Character_Set) return Boolean renames Is_Subset;

   subtype Wide_Character_Sequence is Wide_String;

   function To_Set (Sequence : Wide_Character_Sequence) return Wide_Character_Set;
   function To_Set (Singleton : Wide_Character) return Wide_Character_Set;
   function To_Sequence (Set : Wide_Character_Set) return Wide_Character_Sequence;

   --------------------------------
   -- Wide_Character_Mapping type --
   --------------------------------

   type Wide_Character_Mapping is private;
   pragma Preelaborable_Initialization (Wide_Character_Mapping);

   function Value
     (Map     : Wide_Character_Mapping;
      Element : Wide_Character) return Wide_Character;

   Identity : constant Wide_Character_Mapping;

   function To_Mapping
     (From, To : Wide_Character_Sequence) return Wide_Character_Mapping;

   function To_Domain (Map : Wide_Character_Mapping) return Wide_Character_Sequence;
   function To_Range (Map : Wide_Character_Mapping) return Wide_Character_Sequence;

   type Wide_Character_Mapping_Function is
     access function (From : Wide_Character) return Wide_Character;

private

   type Wide_Character_Set is record
      Chars  : Wide_String (1 .. Max_Set_Size);
      Length : Natural := 0;
   end record;

   Null_Set : constant Wide_Character_Set :=
     (Chars => (others => Wide_Character'Val (0)), Length => 0);

   Max_Map_Size : constant := 64;

   type Wide_Character_Mapping is record
      Domain : Wide_String (1 .. Max_Map_Size);
      Ranges : Wide_String (1 .. Max_Map_Size);
      Length : Natural := 0;
   end record;

   Identity : constant Wide_Character_Mapping :=
     (Domain => (others => Wide_Character'Val (0)),
      Ranges => (others => Wide_Character'Val (0)),
      Length => 0);

end Ada.Strings.Wide_Maps;
