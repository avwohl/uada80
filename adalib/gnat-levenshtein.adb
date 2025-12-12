-- GNAT.Levenshtein body for Z80
-- Levenshtein distance implementation

with Ada.Characters.Handling;

package body GNAT.Levenshtein is

   Max_String_Length : constant := 64;  -- Limited for Z80

   --------------
   -- Distance --
   --------------

   function Distance (S1, S2 : String) return Natural is
      -- Use two-row optimization for Z80 memory constraints
      Len1 : constant Natural := Natural'Min (S1'Length, Max_String_Length);
      Len2 : constant Natural := Natural'Min (S2'Length, Max_String_Length);
      Prev : array (0 .. Max_String_Length) of Natural;
      Curr : array (0 .. Max_String_Length) of Natural;
      Cost : Natural;
   begin
      if Len1 = 0 then
         return Len2;
      end if;
      if Len2 = 0 then
         return Len1;
      end if;

      -- Initialize first row
      for J in 0 .. Len2 loop
         Prev (J) := J;
      end loop;

      -- Fill in the matrix
      for I in 1 .. Len1 loop
         Curr (0) := I;

         for J in 1 .. Len2 loop
            if S1 (S1'First + I - 1) = S2 (S2'First + J - 1) then
               Cost := 0;
            else
               Cost := 1;
            end if;

            Curr (J) := Natural'Min (
              Natural'Min (Curr (J - 1) + 1, Prev (J) + 1),
              Prev (J - 1) + Cost);
         end loop;

         -- Swap rows
         for K in 0 .. Len2 loop
            Prev (K) := Curr (K);
         end loop;
      end loop;

      return Prev (Len2);
   end Distance;

   -------------------------------
   -- Distance_Case_Insensitive --
   -------------------------------

   function Distance_Case_Insensitive (S1, S2 : String) return Natural is
      use Ada.Characters.Handling;
      T1 : String (S1'Range) := S1;
      T2 : String (S2'Range) := S2;
   begin
      for I in T1'Range loop
         T1 (I) := To_Lower (T1 (I));
      end loop;
      for I in T2'Range loop
         T2 (I) := To_Lower (T2 (I));
      end loop;
      return Distance (T1, T2);
   end Distance_Case_Insensitive;

   ----------------
   -- Similarity --
   ----------------

   function Similarity (S1, S2 : String) return Float is
      Max_Len : constant Natural := Natural'Max (S1'Length, S2'Length);
      Dist    : Natural;
   begin
      if Max_Len = 0 then
         return 1.0;
      end if;
      Dist := Distance (S1, S2);
      return 1.0 - Float (Dist) / Float (Max_Len);
   end Similarity;

   ----------------
   -- Is_Similar --
   ----------------

   function Is_Similar
     (S1, S2     : String;
      Max_Dist   : Natural := 2) return Boolean
   is
   begin
      return Distance (S1, S2) <= Max_Dist;
   end Is_Similar;

end GNAT.Levenshtein;
