-- Ada.Tags for Z80
-- Tagged type tag operations
--
-- Provides runtime type identification for tagged types

package Ada.Tags is
   pragma Preelaborate;

   -- Tag type represents the runtime type tag of a tagged type
   type Tag is private;

   No_Tag : constant Tag;

   -- Tag comparison
   function "=" (Left, Right : Tag) return Boolean;

   -- Get tag from object
   function Tag_From_Object (Object : Tagged_Type'Class) return Tag;
   -- Note: This is normally an attribute (Object'Tag), but declared here
   -- for completeness. The compiler handles 'Tag directly.

   -- Tag properties
   function Expanded_Name (T : Tag) return String;
   -- Returns the expanded name of the type

   function External_Tag (T : Tag) return String;
   -- Returns the external tag string

   function Internal_Tag (External : String) return Tag;
   -- Returns the tag from an external tag string

   function Descendant_Tag (External : String; Ancestor : Tag) return Tag;
   -- Returns a descendant tag from an external tag

   function Is_Descendant_At_Same_Level
     (Descendant, Ancestor : Tag) return Boolean;
   -- Check if Descendant is at same level as Ancestor

   function Parent_Tag (T : Tag) return Tag;
   -- Returns the tag of the parent type

   function Interface_Ancestor_Tags (T : Tag) return Tag_Array;
   -- Returns tags of all interface ancestors

   type Tag_Array is array (Positive range <>) of Tag;

   -- Exceptions
   Tag_Error : exception;

private

   -- Tag is implemented as a pointer to a type descriptor
   -- On Z80, we use a 16-bit address
   type Tag is new Integer range 0 .. 65535;

   No_Tag : constant Tag := 0;

   -- Note: Full tagged type support requires compiler support
   -- for generating type descriptors and dispatch tables.

end Ada.Tags;
