-- GNAT.Casing_Utilities for Z80
-- Case manipulation utilities

package GNAT.Casing_Utilities is
   pragma Preelaborate;

   type Casing_Type is
     (All_Upper_Case,
      All_Lower_Case,
      Mixed_Case,
      Unknown);

   function Determine_Casing (Name : String) return Casing_Type;
   --  Determine the casing of a name

   procedure Set_Casing (Name : in Out String; Casing : Casing_Type);
   --  Convert string to specified casing
   --  Mixed_Case applies to identifiers

   procedure Set_All_Upper_Case (Name : in Out String);
   procedure Set_All_Lower_Case (Name : in Out String);
   procedure Set_Mixed_Case (Name : in Out String);

private
   function Is_Upper (C : Character) return Boolean;
   function Is_Lower (C : Character) return Boolean;
   function To_Upper (C : Character) return Character;
   function To_Lower (C : Character) return Character;

end GNAT.Casing_Utilities;
