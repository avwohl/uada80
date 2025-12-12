-- GNAT.Wide_Spelling_Checker body for Z80
-- Wide string spelling implementation

package body GNAT.Wide_Spelling_Checker is

   -------------------------
   -- Is_Bad_Spelling_Of --
   -------------------------

   function Is_Bad_Spelling_Of (Found, Expect : Wide_String) return Boolean is
      Dist : constant Natural := Edit_Distance (Found, Expect);
      Max_Len : constant Natural := Natural'Max (Found'Length, Expect'Length);
   begin
      if Max_Len = 0 then
         return False;
      end if;

      -- Allow up to 1/3 edit distance
      return Dist <= (Max_Len + 2) / 3;
   end Is_Bad_Spelling_Of;

   -------------------
   -- Edit_Distance --
   -------------------

   function Edit_Distance (S1, S2 : Wide_String) return Natural is
      -- Use simplified algorithm for Z80 memory constraints
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

      -- Limit for Z80 memory
      if M > 100 or N > 100 then
         return Natural'Max (M, N);
      end if;

      -- Initialize first row
      for J in 0 .. N loop
         Prev (J) := J;
      end loop;

      -- Dynamic programming
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

end GNAT.Wide_Spelling_Checker;
