-- Ada.Text_IO.Enum_Aux for Z80
-- Enumeration type I/O support

package Ada.Text_IO.Enum_Aux is

   -- Get enumeration from string (generic concept)
   -- Actual implementations use type-specific knowledge

   -- Put enumeration image to string
   procedure Put_Enum
     (To    : out String;
      Image : String;
      Width : Natural;
      Set   : Type_Set := Upper_Case);

   -- Format enumeration image
   function Format_Enum
     (Image : String;
      Width : Natural;
      Set   : Type_Set := Upper_Case) return String;

   -- Convert to uppercase
   procedure To_Upper (S : in Out String);

   -- Convert to lowercase
   procedure To_Lower (S : in Out String);

end Ada.Text_IO.Enum_Aux;
