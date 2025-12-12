-- GNAT.Combination for Z80
-- Combination generation utilities

package GNAT.Combination is
   pragma Pure;

   function Binomial (N, K : Natural) return Natural;
   --  Return N choose K (binomial coefficient)

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
   function Next_Combination
     (A    : in Out Array_Type;
      N    : Positive;
      K    : Positive) return Boolean;
   --  Generate next combination of K elements from N
   --  Returns False when last combination reached

end GNAT.Combination;
