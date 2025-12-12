-- GNAT.Text_Metrics body for Z80
-- Text metrics implementation

package body GNAT.Text_Metrics is

   function Is_Alpha (C : Character) return Boolean is
   begin
      return (C >= 'A' and C <= 'Z') or (C >= 'a' and C <= 'z');
   end Is_Alpha;

   function Is_Digit (C : Character) return Boolean is
   begin
      return C >= '0' and C <= '9';
   end Is_Digit;

   function Is_Space (C : Character) return Boolean is
   begin
      return C = ' ' or C = Character'Val (9) or
             C = Character'Val (10) or C = Character'Val (13);
   end Is_Space;

   function Is_Upper (C : Character) return Boolean is
   begin
      return C >= 'A' and C <= 'Z';
   end Is_Upper;

   function Is_Lower (C : Character) return Boolean is
   begin
      return C >= 'a' and C <= 'z';
   end Is_Lower;

   function Is_Punct (C : Character) return Boolean is
   begin
      return C = '.' or C = ',' or C = '!' or C = '?' or
             C = ';' or C = ':' or C = '-' or C = '"' or C = '''';
   end Is_Punct;

   function Is_Vowel (C : Character) return Boolean is
   begin
      return C = 'a' or C = 'e' or C = 'i' or C = 'o' or C = 'u' or
             C = 'A' or C = 'E' or C = 'I' or C = 'O' or C = 'U';
   end Is_Vowel;

   ----------------
   -- Char_Count --
   ----------------

   function Char_Count (S : String) return Natural is
   begin
      return S'Length;
   end Char_Count;

   ----------------
   -- Word_Count --
   ----------------

   function Word_Count (S : String) return Natural is
      Count   : Natural := 0;
      In_Word : Boolean := False;
   begin
      for C of S loop
         if Is_Space (C) then
            In_Word := False;
         elsif not In_Word then
            In_Word := True;
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Word_Count;

   ----------------
   -- Line_Count --
   ----------------

   function Line_Count (S : String) return Natural is
      Count : Natural := 0;
   begin
      if S'Length = 0 then
         return 0;
      end if;

      Count := 1;
      for C of S loop
         if C = Character'Val (10) then  -- LF
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Line_Count;

   --------------------
   -- Sentence_Count --
   --------------------

   function Sentence_Count (S : String) return Natural is
      Count : Natural := 0;
   begin
      for C of S loop
         if C = '.' or C = '!' or C = '?' then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Sentence_Count;

   ---------------------
   -- Paragraph_Count --
   ---------------------

   function Paragraph_Count (S : String) return Natural is
      Count      : Natural := 0;
      Blank_Line : Boolean := True;
      In_Para    : Boolean := False;
   begin
      for I in S'Range loop
         if S (I) = Character'Val (10) then
            if Blank_Line and In_Para then
               In_Para := False;
            end if;
            Blank_Line := True;
         elsif not Is_Space (S (I)) then
            Blank_Line := False;
            if not In_Para then
               In_Para := True;
               Count := Count + 1;
            end if;
         end if;
      end loop;
      return Count;
   end Paragraph_Count;

   -----------------
   -- Alpha_Count --
   -----------------

   function Alpha_Count (S : String) return Natural is
      Count : Natural := 0;
   begin
      for C of S loop
         if Is_Alpha (C) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Alpha_Count;

   -----------------
   -- Digit_Count --
   -----------------

   function Digit_Count (S : String) return Natural is
      Count : Natural := 0;
   begin
      for C of S loop
         if Is_Digit (C) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Digit_Count;

   -----------------
   -- Space_Count --
   -----------------

   function Space_Count (S : String) return Natural is
      Count : Natural := 0;
   begin
      for C of S loop
         if Is_Space (C) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Space_Count;

   -----------------
   -- Punct_Count --
   -----------------

   function Punct_Count (S : String) return Natural is
      Count : Natural := 0;
   begin
      for C of S loop
         if Is_Punct (C) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Punct_Count;

   -----------------
   -- Upper_Count --
   -----------------

   function Upper_Count (S : String) return Natural is
      Count : Natural := 0;
   begin
      for C of S loop
         if Is_Upper (C) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Upper_Count;

   -----------------
   -- Lower_Count --
   -----------------

   function Lower_Count (S : String) return Natural is
      Count : Natural := 0;
   begin
      for C of S loop
         if Is_Lower (C) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Lower_Count;

   ---------------------
   -- Avg_Word_Length --
   ---------------------

   function Avg_Word_Length (S : String) return Natural is
      Words : constant Natural := Word_Count (S);
      Chars : constant Natural := Alpha_Count (S);
   begin
      if Words = 0 then
         return 0;
      end if;
      return (Chars * 100) / Words;
   end Avg_Word_Length;

   ---------------------
   -- Max_Word_Length --
   ---------------------

   function Max_Word_Length (S : String) return Natural is
      Max_Len : Natural := 0;
      Cur_Len : Natural := 0;
   begin
      for C of S loop
         if Is_Alpha (C) then
            Cur_Len := Cur_Len + 1;
         else
            if Cur_Len > Max_Len then
               Max_Len := Cur_Len;
            end if;
            Cur_Len := 0;
         end if;
      end loop;
      if Cur_Len > Max_Len then
         Max_Len := Cur_Len;
      end if;
      return Max_Len;
   end Max_Word_Length;

   ---------------------
   -- Min_Word_Length --
   ---------------------

   function Min_Word_Length (S : String) return Natural is
      Min_Len : Natural := Natural'Last;
      Cur_Len : Natural := 0;
      Found   : Boolean := False;
   begin
      for C of S loop
         if Is_Alpha (C) then
            Cur_Len := Cur_Len + 1;
         else
            if Cur_Len > 0 then
               Found := True;
               if Cur_Len < Min_Len then
                  Min_Len := Cur_Len;
               end if;
            end if;
            Cur_Len := 0;
         end if;
      end loop;
      if Cur_Len > 0 then
         Found := True;
         if Cur_Len < Min_Len then
            Min_Len := Cur_Len;
         end if;
      end if;
      if Found then
         return Min_Len;
      else
         return 0;
      end if;
   end Min_Word_Length;

   ---------------------
   -- Avg_Line_Length --
   ---------------------

   function Avg_Line_Length (S : String) return Natural is
      Lines : constant Natural := Line_Count (S);
   begin
      if Lines = 0 then
         return 0;
      end if;
      return S'Length / Lines;
   end Avg_Line_Length;

   ---------------------
   -- Max_Line_Length --
   ---------------------

   function Max_Line_Length (S : String) return Natural is
      Max_Len : Natural := 0;
      Cur_Len : Natural := 0;
   begin
      for C of S loop
         if C = Character'Val (10) then
            if Cur_Len > Max_Len then
               Max_Len := Cur_Len;
            end if;
            Cur_Len := 0;
         else
            Cur_Len := Cur_Len + 1;
         end if;
      end loop;
      if Cur_Len > Max_Len then
         Max_Len := Cur_Len;
      end if;
      return Max_Len;
   end Max_Line_Length;

   --------------------
   -- Char_Frequency --
   --------------------

   function Char_Frequency (S : String; C : Character) return Natural is
      Count : Natural := 0;
   begin
      for Ch of S loop
         if Ch = C then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Char_Frequency;

   ----------------------
   -- Most_Common_Char --
   ----------------------

   function Most_Common_Char (S : String) return Character is
      Counts : array (0 .. 127) of Natural := (others => 0);
      Max_Count : Natural := 0;
      Max_Char  : Character := ' ';
   begin
      for C of S loop
         if not Is_Space (C) and Character'Pos (C) <= 127 then
            Counts (Character'Pos (C)) := Counts (Character'Pos (C)) + 1;
         end if;
      end loop;

      for I in 33 .. 127 loop
         if Counts (I) > Max_Count then
            Max_Count := Counts (I);
            Max_Char := Character'Val (I);
         end if;
      end loop;

      return Max_Char;
   end Most_Common_Char;

   ------------------
   -- Unique_Chars --
   ------------------

   function Unique_Chars (S : String) return Natural is
      Seen  : array (0 .. 127) of Boolean := (others => False);
      Count : Natural := 0;
   begin
      for C of S loop
         if Character'Pos (C) <= 127 and then not Seen (Character'Pos (C)) then
            Seen (Character'Pos (C)) := True;
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Unique_Chars;

   --------------------
   -- Syllable_Count --
   --------------------

   function Syllable_Count (S : String) return Natural is
      Count       : Natural := 0;
      Prev_Vowel  : Boolean := False;
      In_Word     : Boolean := False;
      Word_Syl    : Natural := 0;
   begin
      for I in S'Range loop
         if Is_Alpha (S (I)) then
            if not In_Word then
               In_Word := True;
               Word_Syl := 0;
               Prev_Vowel := False;
            end if;

            if Is_Vowel (S (I)) then
               if not Prev_Vowel then
                  Word_Syl := Word_Syl + 1;
               end if;
               Prev_Vowel := True;
            else
               Prev_Vowel := False;
            end if;
         else
            if In_Word then
               -- Handle silent 'e' at end
               if Word_Syl > 1 and I > S'First and then
                  (S (I - 1) = 'e' or S (I - 1) = 'E') then
                  Word_Syl := Word_Syl - 1;
               end if;
               if Word_Syl = 0 then
                  Word_Syl := 1;  -- Every word has at least one syllable
               end if;
               Count := Count + Word_Syl;
               In_Word := False;
            end if;
         end if;
      end loop;

      -- Handle last word
      if In_Word then
         if Word_Syl = 0 then
            Word_Syl := 1;
         end if;
         Count := Count + Word_Syl;
      end if;

      return Count;
   end Syllable_Count;

   ----------------------------
   -- Avg_Syllables_Per_Word --
   ----------------------------

   function Avg_Syllables_Per_Word (S : String) return Natural is
      Words : constant Natural := Word_Count (S);
      Syls  : constant Natural := Syllable_Count (S);
   begin
      if Words = 0 then
         return 0;
      end if;
      return (Syls * 100) / Words;
   end Avg_Syllables_Per_Word;

   -------------------------
   -- Flesch_Reading_Ease --
   -------------------------

   function Flesch_Reading_Ease (S : String) return Integer is
      Words     : constant Natural := Word_Count (S);
      Sentences : constant Natural := Sentence_Count (S);
      Syllables : constant Natural := Syllable_Count (S);
      ASL, ASW  : Integer;
   begin
      if Words = 0 or Sentences = 0 then
         return 0;
      end if;

      -- Average Sentence Length
      ASL := (Words * 100) / Sentences;

      -- Average Syllables per Word
      ASW := (Syllables * 100) / Words;

      -- Flesch formula: 206.835 - 1.015 * ASL - 84.6 * ASW
      -- Simplified with scaled integers
      return 20684 - (ASL * 102) / 100 - (ASW * 846) / 100;
   end Flesch_Reading_Ease;

   -------------------------
   -- Flesch_Kincaid_Grade --
   -------------------------

   function Flesch_Kincaid_Grade (S : String) return Integer is
      Words     : constant Natural := Word_Count (S);
      Sentences : constant Natural := Sentence_Count (S);
      Syllables : constant Natural := Syllable_Count (S);
      ASL, ASW  : Integer;
   begin
      if Words = 0 or Sentences = 0 then
         return 0;
      end if;

      ASL := (Words * 100) / Sentences;
      ASW := (Syllables * 100) / Words;

      -- Grade = 0.39 * ASL + 11.8 * ASW - 15.59
      return ((ASL * 39) / 100 + (ASW * 1180) / 100 - 1559) / 100;
   end Flesch_Kincaid_Grade;

   ----------------
   -- Similarity --
   ----------------

   function Similarity (S1, S2 : String) return Natural is
      Common : Natural := 0;
      Total  : Natural;
   begin
      Total := S1'Length + S2'Length;
      if Total = 0 then
         return 100;
      end if;

      for C of S1 loop
         for D of S2 loop
            if C = D then
               Common := Common + 1;
               exit;
            end if;
         end loop;
      end loop;

      return (Common * 200) / Total;
   end Similarity;

   -------------------
   -- Edit_Distance --
   -------------------

   function Edit_Distance (S1, S2 : String) return Natural is
      -- Simple implementation for short strings (Z80 memory constraint)
      Max_Len : constant := 32;
      D : array (0 .. Max_Len, 0 .. Max_Len) of Natural;
      L1, L2 : Natural;
   begin
      L1 := S1'Length;
      L2 := S2'Length;

      if L1 > Max_Len then L1 := Max_Len; end if;
      if L2 > Max_Len then L2 := Max_Len; end if;

      for I in 0 .. L1 loop
         D (I, 0) := I;
      end loop;

      for J in 0 .. L2 loop
         D (0, J) := J;
      end loop;

      for I in 1 .. L1 loop
         for J in 1 .. L2 loop
            if S1 (S1'First + I - 1) = S2 (S2'First + J - 1) then
               D (I, J) := D (I - 1, J - 1);
            else
               D (I, J) := 1 + Natural'Min (D (I - 1, J),
                               Natural'Min (D (I, J - 1), D (I - 1, J - 1)));
            end if;
         end loop;
      end loop;

      return D (L1, L2);
   end Edit_Distance;

   --------------------
   -- Contains_Digits --
   --------------------

   function Contains_Digits (S : String) return Boolean is
   begin
      for C of S loop
         if Is_Digit (C) then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Digits;

   --------------------
   -- Contains_Alpha --
   --------------------

   function Contains_Alpha (S : String) return Boolean is
   begin
      for C of S loop
         if Is_Alpha (C) then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Alpha;

   --------------------
   -- Contains_Upper --
   --------------------

   function Contains_Upper (S : String) return Boolean is
   begin
      for C of S loop
         if Is_Upper (C) then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Upper;

   --------------------
   -- Contains_Lower --
   --------------------

   function Contains_Lower (S : String) return Boolean is
   begin
      for C of S loop
         if Is_Lower (C) then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Lower;

   -------------------------
   -- Contains_Mixed_Case --
   -------------------------

   function Contains_Mixed_Case (S : String) return Boolean is
   begin
      return Contains_Upper (S) and Contains_Lower (S);
   end Contains_Mixed_Case;

   ------------------
   -- Is_All_Alpha --
   ------------------

   function Is_All_Alpha (S : String) return Boolean is
   begin
      if S'Length = 0 then
         return False;
      end if;
      for C of S loop
         if not Is_Alpha (C) then
            return False;
         end if;
      end loop;
      return True;
   end Is_All_Alpha;

   -------------------
   -- Is_All_Digits --
   -------------------

   function Is_All_Digits (S : String) return Boolean is
   begin
      if S'Length = 0 then
         return False;
      end if;
      for C of S loop
         if not Is_Digit (C) then
            return False;
         end if;
      end loop;
      return True;
   end Is_All_Digits;

   ------------------
   -- Is_All_Alnum --
   ------------------

   function Is_All_Alnum (S : String) return Boolean is
   begin
      if S'Length = 0 then
         return False;
      end if;
      for C of S loop
         if not Is_Alpha (C) and not Is_Digit (C) then
            return False;
         end if;
      end loop;
      return True;
   end Is_All_Alnum;

   ------------------
   -- Is_All_Upper --
   ------------------

   function Is_All_Upper (S : String) return Boolean is
   begin
      if S'Length = 0 then
         return False;
      end if;
      for C of S loop
         if Is_Alpha (C) and not Is_Upper (C) then
            return False;
         end if;
      end loop;
      return Contains_Upper (S);
   end Is_All_Upper;

   ------------------
   -- Is_All_Lower --
   ------------------

   function Is_All_Lower (S : String) return Boolean is
   begin
      if S'Length = 0 then
         return False;
      end if;
      for C of S loop
         if Is_Alpha (C) and not Is_Lower (C) then
            return False;
         end if;
      end loop;
      return Contains_Lower (S);
   end Is_All_Lower;

   --------------------
   -- Is_Capitalized --
   --------------------

   function Is_Capitalized (S : String) return Boolean is
      Found_First : Boolean := False;
   begin
      for C of S loop
         if Is_Alpha (C) then
            if not Found_First then
               if not Is_Upper (C) then
                  return False;
               end if;
               Found_First := True;
            else
               if Is_Upper (C) then
                  return False;
               end if;
            end if;
         end if;
      end loop;
      return Found_First;
   end Is_Capitalized;

   --------------------
   -- Leading_Spaces --
   --------------------

   function Leading_Spaces (S : String) return Natural is
      Count : Natural := 0;
   begin
      for C of S loop
         exit when not Is_Space (C);
         Count := Count + 1;
      end loop;
      return Count;
   end Leading_Spaces;

   ---------------------
   -- Trailing_Spaces --
   ---------------------

   function Trailing_Spaces (S : String) return Natural is
      Count : Natural := 0;
   begin
      for I in reverse S'Range loop
         exit when not Is_Space (S (I));
         Count := Count + 1;
      end loop;
      return Count;
   end Trailing_Spaces;

   -----------------------
   -- Has_Leading_Space --
   -----------------------

   function Has_Leading_Space (S : String) return Boolean is
   begin
      return S'Length > 0 and then Is_Space (S (S'First));
   end Has_Leading_Space;

   ------------------------
   -- Has_Trailing_Space --
   ------------------------

   function Has_Trailing_Space (S : String) return Boolean is
   begin
      return S'Length > 0 and then Is_Space (S (S'Last));
   end Has_Trailing_Space;

end GNAT.Text_Metrics;
