-- Ada.Unchecked_Conversion for Z80
-- Provides type-safe bit-level conversions between types

generic
   type Source is limited private;
   type Target is limited private;
function Ada.Unchecked_Conversion (S : Source) return Target;
pragma Pure (Ada.Unchecked_Conversion);
pragma No_Elaboration_Code_All (Ada.Unchecked_Conversion);
pragma Convention (Intrinsic, Ada.Unchecked_Conversion);
