-- Ada.Strings.Equal_Case_Insensitive body for Z80
-- Case-insensitive string comparison implementation

with Ada.Characters.Handling;

package body Ada.Strings.Equal_Case_Insensitive is

   -----------
   -- Equal --
   -----------

   function Equal (Left, Right : String) return Boolean is
      use Ada.Characters.Handling;
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;

      for I in 0 .. Left'Length - 1 loop
         if To_Lower (Left (Left'First + I)) /= To_Lower (Right (Right'First + I)) then
            return False;
         end if;
      end loop;

      return True;
   end Equal;

end Ada.Strings.Equal_Case_Insensitive;
