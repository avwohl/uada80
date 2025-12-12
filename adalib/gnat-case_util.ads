-- GNAT.Case_Util for Z80
-- Character case conversion utilities

package GNAT.Case_Util is
   pragma Pure;

   -- Convert character to lowercase
   function To_Lower (A : Character) return Character;

   -- Convert character to uppercase
   function To_Upper (A : Character) return Character;

   -- Convert string to lowercase (in place)
   procedure To_Lower (A : in Out String);

   -- Convert string to uppercase (in place)
   procedure To_Upper (A : in Out String);

   -- Convert string to mixed case (capitalize words)
   procedure To_Mixed (A : in Out String);

end GNAT.Case_Util;
