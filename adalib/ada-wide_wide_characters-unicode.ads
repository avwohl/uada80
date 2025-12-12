-- Ada.Wide_Wide_Characters.Unicode for Z80
-- Unicode category information for Wide_Wide_Character
--
-- Provides Unicode general category information

package Ada.Wide_Wide_Characters.Unicode is
   pragma Pure;

   -- Unicode general categories
   type General_Category is
     (Unassigned,
      Uppercase_Letter,
      Lowercase_Letter,
      Titlecase_Letter,
      Modifier_Letter,
      Other_Letter,
      Nonspacing_Mark,
      Spacing_Mark,
      Enclosing_Mark,
      Decimal_Number,
      Letter_Number,
      Other_Number,
      Connector_Punctuation,
      Dash_Punctuation,
      Open_Punctuation,
      Close_Punctuation,
      Initial_Punctuation,
      Final_Punctuation,
      Other_Punctuation,
      Math_Symbol,
      Currency_Symbol,
      Modifier_Symbol,
      Other_Symbol,
      Space_Separator,
      Line_Separator,
      Paragraph_Separator,
      Control,
      Format,
      Surrogate,
      Private_Use);

   -- Get the general category of a character
   function Get_Category (Item : Wide_Wide_Character) return General_Category;

   -- Category predicates
   function Is_Letter (Item : Wide_Wide_Character) return Boolean;
   function Is_Mark (Item : Wide_Wide_Character) return Boolean;
   function Is_Number (Item : Wide_Wide_Character) return Boolean;
   function Is_Punctuation (Item : Wide_Wide_Character) return Boolean;
   function Is_Symbol (Item : Wide_Wide_Character) return Boolean;
   function Is_Separator (Item : Wide_Wide_Character) return Boolean;
   function Is_Other (Item : Wide_Wide_Character) return Boolean;

   -- Line/paragraph break predicates
   function Is_Line_Break (Item : Wide_Wide_Character) return Boolean;
   function Is_Other_Format (Item : Wide_Wide_Character) return Boolean;

   -- Non-graphic character predicate
   function Is_Non_Graphic (Item : Wide_Wide_Character) return Boolean;

end Ada.Wide_Wide_Characters.Unicode;
