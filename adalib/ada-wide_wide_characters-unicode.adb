-- Ada.Wide_Wide_Characters.Unicode body for Z80
-- Unicode category information implementation

package body Ada.Wide_Wide_Characters.Unicode is

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category (Item : Wide_Wide_Character) return General_Category is
      Code : constant Integer := Wide_Wide_Character'Pos (Item);
   begin
      -- Basic ASCII range (simplified for Z80)
      if Code <= 16#1F# or Code = 16#7F# then
         return Control;
      elsif Code = 16#20# then
         return Space_Separator;
      elsif Code in 16#21# .. 16#2F# then
         -- !"#$%&'()*+,-./
         if Code in 16#28# .. 16#29# then
            return Open_Punctuation;
         elsif Code = 16#2D# then
            return Dash_Punctuation;
         else
            return Other_Punctuation;
         end if;
      elsif Code in 16#30# .. 16#39# then
         return Decimal_Number;
      elsif Code in 16#3A# .. 16#40# then
         return Other_Punctuation;
      elsif Code in 16#41# .. 16#5A# then
         return Uppercase_Letter;
      elsif Code in 16#5B# .. 16#60# then
         if Code = 16#5B# then
            return Open_Punctuation;
         elsif Code = 16#5D# then
            return Close_Punctuation;
         elsif Code = 16#5F# then
            return Connector_Punctuation;
         else
            return Other_Punctuation;
         end if;
      elsif Code in 16#61# .. 16#7A# then
         return Lowercase_Letter;
      elsif Code in 16#7B# .. 16#7E# then
         if Code = 16#7B# then
            return Open_Punctuation;
         elsif Code = 16#7D# then
            return Close_Punctuation;
         else
            return Other_Punctuation;
         end if;
      elsif Code in 16#80# .. 16#9F# then
         return Control;
      elsif Code = 16#A0# then
         return Space_Separator;
      elsif Code in 16#A1# .. 16#BF# then
         return Other_Punctuation;
      elsif Code in 16#C0# .. 16#D6# then
         return Uppercase_Letter;
      elsif Code = 16#D7# then
         return Math_Symbol;
      elsif Code in 16#D8# .. 16#DE# then
         return Uppercase_Letter;
      elsif Code in 16#DF# .. 16#F6# then
         return Lowercase_Letter;
      elsif Code = 16#F7# then
         return Math_Symbol;
      elsif Code in 16#F8# .. 16#FF# then
         return Lowercase_Letter;
      else
         -- Beyond Latin-1: simplified categorization
         if Code in 16#100# .. 16#17F# then
            -- Latin Extended-A
            return Uppercase_Letter;
         elsif Code in 16#2000# .. 16#206F# then
            -- General Punctuation
            return Other_Punctuation;
         elsif Code in 16#2070# .. 16#209F# then
            -- Superscripts and Subscripts
            return Other_Number;
         elsif Code in 16#20A0# .. 16#20CF# then
            -- Currency Symbols
            return Currency_Symbol;
         elsif Code in 16#2100# .. 16#214F# then
            -- Letterlike Symbols
            return Other_Symbol;
         elsif Code in 16#2200# .. 16#22FF# then
            -- Mathematical Operators
            return Math_Symbol;
         else
            return Unassigned;
         end if;
      end if;
   end Get_Category;

   ---------------
   -- Is_Letter --
   ---------------

   function Is_Letter (Item : Wide_Wide_Character) return Boolean is
      Cat : constant General_Category := Get_Category (Item);
   begin
      return Cat in Uppercase_Letter | Lowercase_Letter | Titlecase_Letter |
                    Modifier_Letter | Other_Letter;
   end Is_Letter;

   -------------
   -- Is_Mark --
   -------------

   function Is_Mark (Item : Wide_Wide_Character) return Boolean is
      Cat : constant General_Category := Get_Category (Item);
   begin
      return Cat in Nonspacing_Mark | Spacing_Mark | Enclosing_Mark;
   end Is_Mark;

   ---------------
   -- Is_Number --
   ---------------

   function Is_Number (Item : Wide_Wide_Character) return Boolean is
      Cat : constant General_Category := Get_Category (Item);
   begin
      return Cat in Decimal_Number | Letter_Number | Other_Number;
   end Is_Number;

   --------------------
   -- Is_Punctuation --
   --------------------

   function Is_Punctuation (Item : Wide_Wide_Character) return Boolean is
      Cat : constant General_Category := Get_Category (Item);
   begin
      return Cat in Connector_Punctuation | Dash_Punctuation |
                    Open_Punctuation | Close_Punctuation |
                    Initial_Punctuation | Final_Punctuation |
                    Other_Punctuation;
   end Is_Punctuation;

   ---------------
   -- Is_Symbol --
   ---------------

   function Is_Symbol (Item : Wide_Wide_Character) return Boolean is
      Cat : constant General_Category := Get_Category (Item);
   begin
      return Cat in Math_Symbol | Currency_Symbol |
                    Modifier_Symbol | Other_Symbol;
   end Is_Symbol;

   ------------------
   -- Is_Separator --
   ------------------

   function Is_Separator (Item : Wide_Wide_Character) return Boolean is
      Cat : constant General_Category := Get_Category (Item);
   begin
      return Cat in Space_Separator | Line_Separator | Paragraph_Separator;
   end Is_Separator;

   --------------
   -- Is_Other --
   --------------

   function Is_Other (Item : Wide_Wide_Character) return Boolean is
      Cat : constant General_Category := Get_Category (Item);
   begin
      return Cat in Control | Format | Surrogate | Private_Use | Unassigned;
   end Is_Other;

   -------------------
   -- Is_Line_Break --
   -------------------

   function Is_Line_Break (Item : Wide_Wide_Character) return Boolean is
      Code : constant Integer := Wide_Wide_Character'Pos (Item);
   begin
      return Code = 16#0A# or     -- LF
             Code = 16#0D# or     -- CR
             Code = 16#85# or     -- NEL
             Code = 16#2028# or   -- LINE SEPARATOR
             Code = 16#2029#;     -- PARAGRAPH SEPARATOR
   end Is_Line_Break;

   ---------------------
   -- Is_Other_Format --
   ---------------------

   function Is_Other_Format (Item : Wide_Wide_Character) return Boolean is
      Cat : constant General_Category := Get_Category (Item);
   begin
      return Cat = Format;
   end Is_Other_Format;

   --------------------
   -- Is_Non_Graphic --
   --------------------

   function Is_Non_Graphic (Item : Wide_Wide_Character) return Boolean is
      Cat : constant General_Category := Get_Category (Item);
   begin
      return Cat in Control | Format | Surrogate | Private_Use |
                    Line_Separator | Paragraph_Separator | Unassigned;
   end Is_Non_Graphic;

end Ada.Wide_Wide_Characters.Unicode;
