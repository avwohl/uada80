-- System.Wide_String_Ops for Z80
-- Wide string operations (limited support)

package System.Wide_String_Ops is
   pragma Pure;

   type Wide_Character is new Natural range 0 .. 16#FFFF#;
   type Wide_String is array (Positive range <>) of Wide_Character;

   -- Comparison
   function "=" (Left, Right : Wide_String) return Boolean;
   function "<" (Left, Right : Wide_String) return Boolean;
   function "<=" (Left, Right : Wide_String) return Boolean;
   function ">" (Left, Right : Wide_String) return Boolean;
   function ">=" (Left, Right : Wide_String) return Boolean;

   -- Concatenation
   function "&" (Left, Right : Wide_String) return Wide_String;
   function "&" (Left : Wide_String; Right : Wide_Character) return Wide_String;
   function "&" (Left : Wide_Character; Right : Wide_String) return Wide_String;

   -- Length
   function Length (S : Wide_String) return Natural;

   -- Convert to narrow string (ASCII only)
   function To_String (S : Wide_String) return String;

   -- Convert from narrow string
   function To_Wide_String (S : String) return Wide_String;

end System.Wide_String_Ops;
