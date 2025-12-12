-- System.Val_Spec body for Z80
-- Value specification constants

package body System.Val_Spec is

   ---------------------
   -- Is_Base_Marker --
   ---------------------

   function Is_Base_Marker (C : Character) return Boolean is
   begin
      return C = Based_Char or else C = Based_Alt;
   end Is_Base_Marker;

   -------------------------
   -- Is_Exponent_Marker --
   -------------------------

   function Is_Exponent_Marker (C : Character) return Boolean is
   begin
      return C = Exponent_Char_Upper or else C = Exponent_Char_Lower;
   end Is_Exponent_Marker;

end System.Val_Spec;
