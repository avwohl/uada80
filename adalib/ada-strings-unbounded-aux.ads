-- Ada.Strings.Unbounded.Aux for Z80
-- Auxiliary operations for unbounded strings

with Ada.Strings.Unbounded;

package Ada.Strings.Unbounded.Aux is
   pragma Preelaborate;

   procedure Get_String
     (U : Unbounded_String;
      S : out String;
      L : out Natural);
   --  Get string content without allocation

   procedure Set_String
     (U : in Out Unbounded_String;
      S : String);
   --  Set unbounded string from string

   function Get_Big_String
     (U : Unbounded_String) return String;
   --  Get the entire string (for large strings)

end Ada.Strings.Unbounded.Aux;
