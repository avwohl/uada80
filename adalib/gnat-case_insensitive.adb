-- GNAT.Case_Insensitive body for Z80
-- Case-insensitive comparison utilities implementation

with Ada.Characters.Handling;

package body GNAT.Case_Insensitive is

   use Ada.Characters.Handling;

   -----------
   -- Equal --
   -----------

   function Equal (Left, Right : String) return Boolean is
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;
      for I in 0 .. Left'Length - 1 loop
         if To_Lower (Left (Left'First + I)) /=
            To_Lower (Right (Right'First + I))
         then
            return False;
         end if;
      end loop;
      return True;
   end Equal;

   ----------
   -- Less --
   ----------

   function Less (Left, Right : String) return Boolean is
      Min_Len : constant Natural := Natural'Min (Left'Length, Right'Length);
      L, R    : Character;
   begin
      for I in 0 .. Min_Len - 1 loop
         L := To_Lower (Left (Left'First + I));
         R := To_Lower (Right (Right'First + I));
         if L < R then
            return True;
         elsif L > R then
            return False;
         end if;
      end loop;
      return Left'Length < Right'Length;
   end Less;

   ----------
   -- Hash --
   ----------

   function Hash (Key : String) return Natural is
      Result : Natural := 0;
   begin
      for I in Key'Range loop
         Result := Result * 31 + Character'Pos (To_Lower (Key (I)));
         Result := Result mod 65521;
      end loop;
      return Result;
   end Hash;

end GNAT.Case_Insensitive;
