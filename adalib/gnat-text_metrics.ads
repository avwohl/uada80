-- GNAT.Text_Metrics for Z80
-- Text analysis and metrics calculation

package GNAT.Text_Metrics is
   pragma Pure;

   -- Character counting
   function Char_Count (S : String) return Natural;
   function Word_Count (S : String) return Natural;
   function Line_Count (S : String) return Natural;
   function Sentence_Count (S : String) return Natural;
   function Paragraph_Count (S : String) return Natural;

   -- Character type counting
   function Alpha_Count (S : String) return Natural;
   function Digit_Count (S : String) return Natural;
   function Space_Count (S : String) return Natural;
   function Punct_Count (S : String) return Natural;
   function Upper_Count (S : String) return Natural;
   function Lower_Count (S : String) return Natural;

   -- Word metrics
   function Avg_Word_Length (S : String) return Natural;  -- Returns hundredths
   function Max_Word_Length (S : String) return Natural;
   function Min_Word_Length (S : String) return Natural;

   -- Line metrics
   function Avg_Line_Length (S : String) return Natural;
   function Max_Line_Length (S : String) return Natural;

   -- Character frequency (returns count of specific char)
   function Char_Frequency (S : String; C : Character) return Natural;

   -- Most common character (excluding spaces)
   function Most_Common_Char (S : String) return Character;

   -- Unique character count
   function Unique_Chars (S : String) return Natural;

   -- Readability metrics (simplified for Z80)
   -- Returns scaled values (multiply by 100 for display)

   -- Flesch Reading Ease (simplified)
   -- Higher = easier to read (0-100 typical range)
   function Flesch_Reading_Ease (S : String) return Integer;

   -- Flesch-Kincaid Grade Level (simplified)
   -- Returns approximate US grade level
   function Flesch_Kincaid_Grade (S : String) return Integer;

   -- Syllable counting (approximate for English)
   function Syllable_Count (S : String) return Natural;
   function Avg_Syllables_Per_Word (S : String) return Natural;  -- Hundredths

   -- Text similarity (simple character-based)
   -- Returns percentage (0-100)
   function Similarity (S1, S2 : String) return Natural;

   -- Levenshtein edit distance
   function Edit_Distance (S1, S2 : String) return Natural;

   -- Contains checks
   function Contains_Digits (S : String) return Boolean;
   function Contains_Alpha (S : String) return Boolean;
   function Contains_Upper (S : String) return Boolean;
   function Contains_Lower (S : String) return Boolean;
   function Contains_Mixed_Case (S : String) return Boolean;

   -- Pattern checks
   function Is_All_Alpha (S : String) return Boolean;
   function Is_All_Digits (S : String) return Boolean;
   function Is_All_Alnum (S : String) return Boolean;
   function Is_All_Upper (S : String) return Boolean;
   function Is_All_Lower (S : String) return Boolean;
   function Is_Capitalized (S : String) return Boolean;

   -- Whitespace analysis
   function Leading_Spaces (S : String) return Natural;
   function Trailing_Spaces (S : String) return Natural;
   function Has_Leading_Space (S : String) return Boolean;
   function Has_Trailing_Space (S : String) return Boolean;

end GNAT.Text_Metrics;
