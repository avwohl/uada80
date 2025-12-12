-- System.Case_Util for Z80
-- Case conversion utilities

package System.Case_Util is
   pragma Pure;

   -- Convert character to uppercase
   function To_Upper (C : Character) return Character;

   -- Convert character to lowercase
   function To_Lower (C : Character) return Character;

   -- Convert string to uppercase in place
   procedure To_Upper (S : in out String);

   -- Convert string to lowercase in place
   procedure To_Lower (S : in Out String);

   -- Check if character is a letter
   function Is_Letter (C : Character) return Boolean;

   -- Check if character is uppercase
   function Is_Upper (C : Character) return Boolean;

   -- Check if character is lowercase
   function Is_Lower (C : Character) return Boolean;

end System.Case_Util;
