-- GNAT.String_Comparison body for Z80
-- Various string comparison implementations

package body GNAT.String_Comparison is

   function To_Upper (C : Character) return Character is
   begin
      if C in 'a' .. 'z' then
         return Character'Val (Character'Pos (C) - 32);
      else
         return C;
      end if;
   end To_Upper;

   -----------
   -- Equal --
   -----------

   function Equal (Left, Right : String) return Boolean is
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;
      for I in Left'Range loop
         if Left (I) /= Right (Right'First + I - Left'First) then
            return False;
         end if;
      end loop;
      return True;
   end Equal;

   -----------------------------
   -- Equal_Case_Insensitive --
   -----------------------------

   function Equal_Case_Insensitive (Left, Right : String) return Boolean is
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;
      for I in Left'Range loop
         if To_Upper (Left (I)) /= To_Upper (Right (Right'First + I - Left'First)) then
            return False;
         end if;
      end loop;
      return True;
   end Equal_Case_Insensitive;

   ---------------
   -- Less_Than --
   ---------------

   function Less_Than (Left, Right : String) return Boolean is
   begin
      return Left < Right;
   end Less_Than;

   ---------------------------------
   -- Less_Than_Case_Insensitive --
   ---------------------------------

   function Less_Than_Case_Insensitive (Left, Right : String) return Boolean is
      Min_Len : constant Natural := Natural'Min (Left'Length, Right'Length);
   begin
      for I in 1 .. Min_Len loop
         declare
            L : constant Character := To_Upper (Left (Left'First + I - 1));
            R : constant Character := To_Upper (Right (Right'First + I - 1));
         begin
            if L < R then
               return True;
            elsif L > R then
               return False;
            end if;
         end;
      end loop;
      return Left'Length < Right'Length;
   end Less_Than_Case_Insensitive;

   -----------------
   -- Starts_With --
   -----------------

   function Starts_With (Source, Prefix : String) return Boolean is
   begin
      if Prefix'Length > Source'Length then
         return False;
      end if;
      return Source (Source'First .. Source'First + Prefix'Length - 1) = Prefix;
   end Starts_With;

   -----------------------------------
   -- Starts_With_Case_Insensitive --
   -----------------------------------

   function Starts_With_Case_Insensitive (Source, Prefix : String) return Boolean is
   begin
      if Prefix'Length > Source'Length then
         return False;
      end if;
      return Equal_Case_Insensitive (
        Source (Source'First .. Source'First + Prefix'Length - 1), Prefix);
   end Starts_With_Case_Insensitive;

   ---------------
   -- Ends_With --
   ---------------

   function Ends_With (Source, Suffix : String) return Boolean is
   begin
      if Suffix'Length > Source'Length then
         return False;
      end if;
      return Source (Source'Last - Suffix'Length + 1 .. Source'Last) = Suffix;
   end Ends_With;

   ---------------------------------
   -- Ends_With_Case_Insensitive --
   ---------------------------------

   function Ends_With_Case_Insensitive (Source, Suffix : String) return Boolean is
   begin
      if Suffix'Length > Source'Length then
         return False;
      end if;
      return Equal_Case_Insensitive (
        Source (Source'Last - Suffix'Length + 1 .. Source'Last), Suffix);
   end Ends_With_Case_Insensitive;

   --------------
   -- Contains --
   --------------

   function Contains (Source, Pattern : String) return Boolean is
   begin
      if Pattern'Length = 0 then
         return True;
      end if;
      if Pattern'Length > Source'Length then
         return False;
      end if;

      for I in Source'First .. Source'Last - Pattern'Length + 1 loop
         if Source (I .. I + Pattern'Length - 1) = Pattern then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   --------------------------------
   -- Contains_Case_Insensitive --
   --------------------------------

   function Contains_Case_Insensitive (Source, Pattern : String) return Boolean is
   begin
      if Pattern'Length = 0 then
         return True;
      end if;
      if Pattern'Length > Source'Length then
         return False;
      end if;

      for I in Source'First .. Source'Last - Pattern'Length + 1 loop
         if Equal_Case_Insensitive (Source (I .. I + Pattern'Length - 1), Pattern) then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Case_Insensitive;

   ---------------------
   -- Natural_Compare --
   ---------------------

   function Natural_Compare (Left, Right : String) return Integer is
      L_Idx : Positive := Left'First;
      R_Idx : Positive := Right'First;
   begin
      while L_Idx <= Left'Last and R_Idx <= Right'Last loop
         if Is_Digit (Left (L_Idx)) and Is_Digit (Right (R_Idx)) then
            -- Compare numeric sequences
            declare
               L_Num : Natural := 0;
               R_Num : Natural := 0;
            begin
               while L_Idx <= Left'Last and then Is_Digit (Left (L_Idx)) loop
                  L_Num := L_Num * 10 + Character'Pos (Left (L_Idx)) - Character'Pos ('0');
                  L_Idx := L_Idx + 1;
               end loop;

               while R_Idx <= Right'Last and then Is_Digit (Right (R_Idx)) loop
                  R_Num := R_Num * 10 + Character'Pos (Right (R_Idx)) - Character'Pos ('0');
                  R_Idx := R_Idx + 1;
               end loop;

               if L_Num < R_Num then
                  return -1;
               elsif L_Num > R_Num then
                  return 1;
               end if;
            end;
         else
            if Left (L_Idx) < Right (R_Idx) then
               return -1;
            elsif Left (L_Idx) > Right (R_Idx) then
               return 1;
            end if;
            L_Idx := L_Idx + 1;
            R_Idx := R_Idx + 1;
         end if;
      end loop;

      if L_Idx > Left'Last and R_Idx > Right'Last then
         return 0;
      elsif L_Idx > Left'Last then
         return -1;
      else
         return 1;
      end if;
   end Natural_Compare;

   -----------------------
   -- Natural_Less_Than --
   -----------------------

   function Natural_Less_Than (Left, Right : String) return Boolean is
   begin
      return Natural_Compare (Left, Right) < 0;
   end Natural_Less_Than;

   --------------------
   -- Match_Wildcard --
   --------------------

   function Match_Wildcard (Source, Pattern : String) return Boolean is
      S_Idx : Natural := Source'First;
      P_Idx : Natural := Pattern'First;
      Star_Idx : Natural := 0;
      Match_Idx : Natural := 0;
   begin
      while S_Idx <= Source'Last loop
         if P_Idx <= Pattern'Last and then
           (Pattern (P_Idx) = '?' or Pattern (P_Idx) = Source (S_Idx))
         then
            S_Idx := S_Idx + 1;
            P_Idx := P_Idx + 1;
         elsif P_Idx <= Pattern'Last and then Pattern (P_Idx) = '*' then
            Star_Idx := P_Idx;
            Match_Idx := S_Idx;
            P_Idx := P_Idx + 1;
         elsif Star_Idx > 0 then
            P_Idx := Star_Idx + 1;
            Match_Idx := Match_Idx + 1;
            S_Idx := Match_Idx;
         else
            return False;
         end if;
      end loop;

      while P_Idx <= Pattern'Last and then Pattern (P_Idx) = '*' loop
         P_Idx := P_Idx + 1;
      end loop;

      return P_Idx > Pattern'Last;
   end Match_Wildcard;

   --------------------------
   -- Common_Prefix_Length --
   --------------------------

   function Common_Prefix_Length (Left, Right : String) return Natural is
      Count : Natural := 0;
      Min_Len : constant Natural := Natural'Min (Left'Length, Right'Length);
   begin
      for I in 1 .. Min_Len loop
         if Left (Left'First + I - 1) = Right (Right'First + I - 1) then
            Count := Count + 1;
         else
            exit;
         end if;
      end loop;
      return Count;
   end Common_Prefix_Length;

   --------------------------
   -- Common_Suffix_Length --
   --------------------------

   function Common_Suffix_Length (Left, Right : String) return Natural is
      Count : Natural := 0;
      Min_Len : constant Natural := Natural'Min (Left'Length, Right'Length);
   begin
      for I in 1 .. Min_Len loop
         if Left (Left'Last - I + 1) = Right (Right'Last - I + 1) then
            Count := Count + 1;
         else
            exit;
         end if;
      end loop;
      return Count;
   end Common_Suffix_Length;

   ----------------------
   -- Hamming_Distance --
   ----------------------

   function Hamming_Distance (Left, Right : String) return Natural is
      Count : Natural := 0;
   begin
      if Left'Length /= Right'Length then
         return Natural'Last;
      end if;

      for I in Left'Range loop
         if Left (I) /= Right (Right'First + I - Left'First) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Hamming_Distance;

   ----------------------
   -- Equal_Normalized --
   ----------------------

   function Equal_Normalized (Left, Right : String) return Boolean is
      L_Idx : Positive := Left'First;
      R_Idx : Positive := Right'First;

      procedure Skip_Whitespace (S : String; Idx : in Out Positive) is
      begin
         while Idx <= S'Last and then Is_Whitespace (S (Idx)) loop
            Idx := Idx + 1;
         end loop;
      end Skip_Whitespace;

   begin
      Skip_Whitespace (Left, L_Idx);
      Skip_Whitespace (Right, R_Idx);

      while L_Idx <= Left'Last and R_Idx <= Right'Last loop
         if Is_Whitespace (Left (L_Idx)) and Is_Whitespace (Right (R_Idx)) then
            Skip_Whitespace (Left, L_Idx);
            Skip_Whitespace (Right, R_Idx);
         elsif Left (L_Idx) /= Right (R_Idx) then
            return False;
         else
            L_Idx := L_Idx + 1;
            R_Idx := R_Idx + 1;
         end if;
      end loop;

      Skip_Whitespace (Left, L_Idx);
      Skip_Whitespace (Right, R_Idx);

      return L_Idx > Left'Last and R_Idx > Right'Last;
   end Equal_Normalized;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (S : String) return Boolean is
   begin
      return S'Length = 0;
   end Is_Empty;

   --------------
   -- Is_Blank --
   --------------

   function Is_Blank (S : String) return Boolean is
   begin
      for C of S loop
         if not Is_Whitespace (C) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Blank;

   -------------------
   -- Is_Whitespace --
   -------------------

   function Is_Whitespace (C : Character) return Boolean is
   begin
      return C = ' ' or C = ASCII.HT or C = ASCII.CR or C = ASCII.LF;
   end Is_Whitespace;

   --------------
   -- Is_Alpha --
   --------------

   function Is_Alpha (C : Character) return Boolean is
   begin
      return C in 'A' .. 'Z' or C in 'a' .. 'z';
   end Is_Alpha;

   --------------
   -- Is_Digit --
   --------------

   function Is_Digit (C : Character) return Boolean is
   begin
      return C in '0' .. '9';
   end Is_Digit;

   --------------------
   -- Is_Alphanumeric --
   --------------------

   function Is_Alphanumeric (C : Character) return Boolean is
   begin
      return Is_Alpha (C) or Is_Digit (C);
   end Is_Alphanumeric;

   ---------------
   -- All_Alpha --
   ---------------

   function All_Alpha (S : String) return Boolean is
   begin
      for C of S loop
         if not Is_Alpha (C) then
            return False;
         end if;
      end loop;
      return True;
   end All_Alpha;

   ---------------
   -- All_Digit --
   ---------------

   function All_Digit (S : String) return Boolean is
   begin
      for C of S loop
         if not Is_Digit (C) then
            return False;
         end if;
      end loop;
      return True;
   end All_Digit;

   ---------------------
   -- All_Alphanumeric --
   ---------------------

   function All_Alphanumeric (S : String) return Boolean is
   begin
      for C of S loop
         if not Is_Alphanumeric (C) then
            return False;
         end if;
      end loop;
      return True;
   end All_Alphanumeric;

end GNAT.String_Comparison;
