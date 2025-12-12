-- GNAT.UTF_32 body for Z80
-- UTF-32 character handling implementation

package body GNAT.UTF_32 is

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category (U : UTF_32) return Category is
   begin
      -- Simplified categorization for ASCII range
      if U < 32 then
         return Cc;  -- Control
      elsif U < 127 then
         if U in 48 .. 57 then
            return Nd;  -- Digit
         elsif U in 65 .. 90 then
            return Lu;  -- Uppercase
         elsif U in 97 .. 122 then
            return Ll;  -- Lowercase
         elsif U = 32 then
            return Zs;  -- Space
         else
            return Po;  -- Other punctuation
         end if;
      elsif U = 127 then
         return Cc;  -- DEL
      else
         return Lo;  -- Other letter (default for non-ASCII)
      end if;
   end Get_Category;

   ---------------
   -- Is_Letter --
   ---------------

   function Is_Letter (U : UTF_32) return Boolean is
      Cat : constant Category := Get_Category (U);
   begin
      return Cat in Ll | Lm | Lo | Lt | Lu;
   end Is_Letter;

   --------------
   -- Is_Digit --
   --------------

   function Is_Digit (U : UTF_32) return Boolean is
   begin
      return Get_Category (U) = Nd;
   end Is_Digit;

   ------------------------
   -- Is_Letter_Or_Digit --
   ------------------------

   function Is_Letter_Or_Digit (U : UTF_32) return Boolean is
   begin
      return Is_Letter (U) or else Is_Digit (U);
   end Is_Letter_Or_Digit;

   ------------------------
   -- Is_Line_Terminator --
   ------------------------

   function Is_Line_Terminator (U : UTF_32) return Boolean is
   begin
      return U = 10 or else U = 13 or else U = 16#85# or else
             U = 16#2028# or else U = 16#2029#;
   end Is_Line_Terminator;

   -------------
   -- Is_Mark --
   -------------

   function Is_Mark (U : UTF_32) return Boolean is
      Cat : constant Category := Get_Category (U);
   begin
      return Cat in Mc | Me | Mn;
   end Is_Mark;

   --------------
   -- Is_Other --
   --------------

   function Is_Other (U : UTF_32) return Boolean is
      Cat : constant Category := Get_Category (U);
   begin
      return Cat in Cc | Cf | Cn | Co | Cs;
   end Is_Other;

   --------------------
   -- Is_Punctuation --
   --------------------

   function Is_Punctuation (U : UTF_32) return Boolean is
      Cat : constant Category := Get_Category (U);
   begin
      return Cat in Pc | Pd | Pe | Pf | Pi | Po | Ps;
   end Is_Punctuation;

   ------------------
   -- Is_Separator --
   ------------------

   function Is_Separator (U : UTF_32) return Boolean is
      Cat : constant Category := Get_Category (U);
   begin
      return Cat in Zl | Zp | Zs;
   end Is_Separator;

   ---------------
   -- Is_Symbol --
   ---------------

   function Is_Symbol (U : UTF_32) return Boolean is
      Cat : constant Category := Get_Category (U);
   begin
      return Cat in Sc | Sk | Sm | So;
   end Is_Symbol;

   -------------------
   -- To_Lower_Case --
   -------------------

   function To_Lower_Case (U : UTF_32) return UTF_32 is
   begin
      if U in 65 .. 90 then
         return U + 32;
      else
         return U;
      end if;
   end To_Lower_Case;

   -------------------
   -- To_Upper_Case --
   -------------------

   function To_Upper_Case (U : UTF_32) return UTF_32 is
   begin
      if U in 97 .. 122 then
         return U - 32;
      else
         return U;
      end if;
   end To_Upper_Case;

end GNAT.UTF_32;
