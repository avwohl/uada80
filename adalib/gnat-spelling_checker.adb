-- GNAT.Spelling_Checker body for Z80
-- String spelling implementation

package body GNAT.Spelling_Checker is

   -------------------------
   -- Is_Bad_Spelling_Of --
   -------------------------

   function Is_Bad_Spelling_Of (Found, Expect : String) return Boolean is
      Dist : constant Natural := Edit_Distance (Found, Expect);
      Max_Len : constant Natural := Natural'Max (Found'Length, Expect'Length);
   begin
      if Max_Len = 0 then
         return False;
      end if;

      return Dist <= (Max_Len + 2) / 3;
   end Is_Bad_Spelling_Of;

   -------------------
   -- Edit_Distance --
   -------------------

   function Edit_Distance (S1, S2 : String) return Natural is
      M : constant Natural := S1'Length;
      N : constant Natural := S2'Length;
      Prev, Curr : array (0 .. 100) of Natural;
      Cost : Natural;
   begin
      if M = 0 then
         return N;
      end if;
      if N = 0 then
         return M;
      end if;

      if M > 100 or N > 100 then
         return Natural'Max (M, N);
      end if;

      for J in 0 .. N loop
         Prev (J) := J;
      end loop;

      for I in 1 .. M loop
         Curr (0) := I;

         for J in 1 .. N loop
            if S1 (S1'First + I - 1) = S2 (S2'First + J - 1) then
               Cost := 0;
            else
               Cost := 1;
            end if;

            Curr (J) := Natural'Min (
               Natural'Min (Curr (J - 1) + 1, Prev (J) + 1),
               Prev (J - 1) + Cost);
         end loop;

         Prev (0 .. N) := Curr (0 .. N);
      end loop;

      return Curr (N);
   end Edit_Distance;

end GNAT.Spelling_Checker;
