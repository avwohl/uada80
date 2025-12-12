-- GNAT.Spitbol.Patterns for Z80
-- SPITBOL-style pattern matching

package GNAT.Spitbol.Patterns is
   pragma Preelaborate;

   -- Pattern type (simplified implementation)
   type Pattern is private;

   -- Null pattern
   Null_Pattern : constant Pattern;

   -- Basic patterns
   function Any (Str : String) return Pattern;
   -- Matches any single character in Str

   function Break (Str : String) return Pattern;
   -- Matches up to (but not including) any character in Str

   function Span (Str : String) return Pattern;
   -- Matches one or more characters in Str

   function Notany (Str : String) return Pattern;
   -- Matches any single character NOT in Str

   function Len (N : Natural) return Pattern;
   -- Matches exactly N characters

   function Arb return Pattern;
   -- Matches arbitrary characters (zero or more)

   function Arbno (P : Pattern) return Pattern;
   -- Matches zero or more occurrences of P

   function Fail return Pattern;
   -- Always fails

   function Succeed return Pattern;
   -- Always succeeds

   function Fence return Pattern;
   -- Prevents backtracking

   -- Pattern operations
   function "&" (L, R : Pattern) return Pattern;
   -- Concatenation

   function Or_Else (L, R : Pattern) return Pattern;
   -- Alternation

   -- Match function
   function Match
     (Subject : String;
      Pat     : Pattern) return Boolean;

   function Match
     (Subject : String;
      Pat     : Pattern;
      Start   : out Natural;
      Stop    : out Natural) return Boolean;

private

   Max_Pattern_Nodes : constant := 16;

   type Pattern_Kind is (
      P_Null, P_Any, P_Break, P_Span, P_Notany,
      P_Len, P_Arb, P_Arbno, P_Fail, P_Succeed, P_Fence,
      P_Concat, P_Alt);

   type Pattern_Node;
   type Pattern_Node_Access is access Pattern_Node;

   type Pattern_Node is record
      Kind   : Pattern_Kind := P_Null;
      Chars  : String (1 .. 32);
      Length : Natural := 0;
      Count  : Natural := 0;
      Left   : Pattern_Node_Access := null;
      Right  : Pattern_Node_Access := null;
   end record;

   type Pattern is record
      Root : Pattern_Node_Access := null;
   end record;

   Null_Pattern : constant Pattern := (Root => null);

end GNAT.Spitbol.Patterns;
