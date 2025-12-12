-- GNAT.UTF_32 for Z80
-- UTF-32 character handling

package GNAT.UTF_32 is
   pragma Pure;

   -- UTF-32 code point
   type UTF_32 is range 0 .. 16#10FFFF#;

   -- Character categories
   type Category is (
      Cc,  -- Other, Control
      Cf,  -- Other, Format
      Cn,  -- Other, Not Assigned
      Co,  -- Other, Private Use
      Cs,  -- Other, Surrogate
      Ll,  -- Letter, Lowercase
      Lm,  -- Letter, Modifier
      Lo,  -- Letter, Other
      Lt,  -- Letter, Titlecase
      Lu,  -- Letter, Uppercase
      Mc,  -- Mark, Spacing Combining
      Me,  -- Mark, Enclosing
      Mn,  -- Mark, Non-Spacing
      Nd,  -- Number, Decimal Digit
      Nl,  -- Number, Letter
      No,  -- Number, Other
      Pc,  -- Punctuation, Connector
      Pd,  -- Punctuation, Dash
      Pe,  -- Punctuation, Close
      Pf,  -- Punctuation, Final quote
      Pi,  -- Punctuation, Initial quote
      Po,  -- Punctuation, Other
      Ps,  -- Punctuation, Open
      Sc,  -- Symbol, Currency
      Sk,  -- Symbol, Modifier
      Sm,  -- Symbol, Math
      So,  -- Symbol, Other
      Zl,  -- Separator, Line
      Zp,  -- Separator, Paragraph
      Zs); -- Separator, Space

   -- Get category of code point
   function Get_Category (U : UTF_32) return Category;

   -- Character classification
   function Is_Letter (U : UTF_32) return Boolean;
   function Is_Digit (U : UTF_32) return Boolean;
   function Is_Letter_Or_Digit (U : UTF_32) return Boolean;
   function Is_Line_Terminator (U : UTF_32) return Boolean;
   function Is_Mark (U : UTF_32) return Boolean;
   function Is_Other (U : UTF_32) return Boolean;
   function Is_Punctuation (U : UTF_32) return Boolean;
   function Is_Separator (U : UTF_32) return Boolean;
   function Is_Symbol (U : UTF_32) return Boolean;

   -- Case conversion
   function To_Lower_Case (U : UTF_32) return UTF_32;
   function To_Upper_Case (U : UTF_32) return UTF_32;

end GNAT.UTF_32;
