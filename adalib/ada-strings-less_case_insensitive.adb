-- Ada.Strings.Less_Case_Insensitive body for Z80
-- Case-insensitive string ordering implementation

with Ada.Characters.Handling;

package body Ada.Strings.Less_Case_Insensitive is

   ----------
   -- Less --
   ----------

   function Less (Left, Right : String) return Boolean is
      use Ada.Characters.Handling;
      L_Char, R_Char : Character;
   begin
      for I in 0 .. Natural'Min (Left'Length, Right'Length) - 1 loop
         L_Char := To_Lower (Left (Left'First + I));
         R_Char := To_Lower (Right (Right'First + I));
         if L_Char < R_Char then
            return True;
         elsif L_Char > R_Char then
            return False;
         end if;
      end loop;

      -- Equal up to minimum length, shorter string is "less"
      return Left'Length < Right'Length;
   end Less;

end Ada.Strings.Less_Case_Insensitive;
