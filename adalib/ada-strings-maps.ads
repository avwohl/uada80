-- Ada.Strings.Maps for Z80
-- Character set and mapping operations

package Ada.Strings.Maps is
   pragma Preelaborate;

   -- Character Set type - represents a set of characters
   -- Implemented as a 256-bit bitmap (32 bytes)
   type Character_Set is private;

   Null_Set : constant Character_Set;

   -- Character Mapping type - maps characters to characters
   type Character_Mapping is private;

   Identity : constant Character_Mapping;

   -- Character Set operations
   function To_Set (Span : Character_Range) return Character_Set;
   function To_Set (Ranges : Character_Ranges) return Character_Set;
   function To_Set (Sequence : Character_Sequence) return Character_Set;
   function To_Set (Singleton : Character) return Character_Set;

   function To_Ranges (Set : Character_Set) return Character_Ranges;
   function To_Sequence (Set : Character_Set) return Character_Sequence;

   -- Set membership
   function Is_In (Element : Character; Set : Character_Set) return Boolean;
   function Is_Subset (Elements : Character_Set; Of_Set : Character_Set) return Boolean;

   -- Set operations
   function "or" (Left, Right : Character_Set) return Character_Set;
   function "and" (Left, Right : Character_Set) return Character_Set;
   function "xor" (Left, Right : Character_Set) return Character_Set;
   function "-" (Left, Right : Character_Set) return Character_Set;
   function "not" (Right : Character_Set) return Character_Set;

   function "=" (Left, Right : Character_Set) return Boolean;

   -- Character Mapping operations
   function To_Mapping (From, To : Character_Sequence) return Character_Mapping;
   function To_Domain (Map : Character_Mapping) return Character_Sequence;
   function To_Range (Map : Character_Mapping) return Character_Sequence;
   function Value (Map : Character_Mapping; Element : Character) return Character;

   -- Predefined Character_Ranges type
   type Character_Range is record
      Low  : Character;
      High : Character;
   end record;

   type Character_Ranges is array (Positive range <>) of Character_Range;

   -- Character_Sequence is just String
   subtype Character_Sequence is String;

private

   -- Character_Set as 256-bit bitmap (one bit per character)
   type Bit_Array is array (0 .. 255) of Boolean;
   pragma Pack (Bit_Array);

   type Character_Set is record
      Bits : Bit_Array := (others => False);
   end record;

   Null_Set : constant Character_Set := (Bits => (others => False));

   -- Character_Mapping as a 256-element array
   type Char_Array is array (Character) of Character;

   type Character_Mapping is record
      Map : Char_Array;
   end record;

   -- Identity mapping initialization
   function Init_Identity return Character_Mapping;

   Identity : constant Character_Mapping := Init_Identity;

end Ada.Strings.Maps;
