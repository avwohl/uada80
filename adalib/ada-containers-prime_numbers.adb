-- Ada.Containers.Prime_Numbers body for Z80
-- Prime number utilities implementation

package body Ada.Containers.Prime_Numbers is

   -- Table of primes for common hash table sizes
   Primes : constant array (1 .. 20) of Count_Type :=
     (5, 11, 23, 47, 97, 197, 397, 797, 1597, 3203,
      6421, 12853, 25717, 51437, 102877, 205759,
      411527, 823117, 1646237, 3292489);

   --------------
   -- Is_Prime --
   --------------

   function Is_Prime (N : Count_Type) return Boolean is
      I : Count_Type := 2;
   begin
      if N < 2 then
         return False;
      end if;
      if N = 2 then
         return True;
      end if;
      if N mod 2 = 0 then
         return False;
      end if;

      I := 3;
      while I * I <= N loop
         if N mod I = 0 then
            return False;
         end if;
         I := I + 2;
      end loop;

      return True;
   end Is_Prime;

   --------------
   -- To_Prime --
   --------------

   function To_Prime (Size : Count_Type) return Count_Type is
   begin
      -- First check the table
      for P of Primes loop
         if P >= Size then
            return P;
         end if;
      end loop;

      -- Search for next prime after last table entry
      declare
         N : Count_Type := Size;
      begin
         if N mod 2 = 0 then
            N := N + 1;
         end if;

         while not Is_Prime (N) loop
            N := N + 2;
         end loop;

         return N;
      end;
   end To_Prime;

end Ada.Containers.Prime_Numbers;
