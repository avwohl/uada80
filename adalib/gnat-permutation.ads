-- GNAT.Permutation for Z80
-- Permutation generation utilities

package GNAT.Permutation is
   pragma Pure;

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
   function Next_Permutation (A : in Out Array_Type) return Boolean;
   --  Generate next lexicographic permutation
   --  Returns False when last permutation reached

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   procedure Sort_For_Permutation (A : in Out Array_Type);
   --  Sort array to start permutation sequence

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
   function Factorial (N : Natural) return Natural;
   --  Return N! (number of permutations)

end GNAT.Permutation;
