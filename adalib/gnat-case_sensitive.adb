-- GNAT.Case_Sensitive body for Z80
-- Case-sensitive comparison utilities implementation

package body GNAT.Case_Sensitive is

   -----------
   -- Equal --
   -----------

   function Equal (Left, Right : String) return Boolean is
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;
      for I in 0 .. Left'Length - 1 loop
         if Left (Left'First + I) /= Right (Right'First + I) then
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
   begin
      for I in 0 .. Min_Len - 1 loop
         if Left (Left'First + I) < Right (Right'First + I) then
            return True;
         elsif Left (Left'First + I) > Right (Right'First + I) then
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
         Result := Result * 31 + Character'Pos (Key (I));
         Result := Result mod 65521;
      end loop;
      return Result;
   end Hash;

end GNAT.Case_Sensitive;
