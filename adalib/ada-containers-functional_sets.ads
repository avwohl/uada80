-- Ada.Containers.Functional_Sets for Z80
-- Functional (immutable) sets

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
   with function "<" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Functional_Sets is
   pragma Pure;

   type Set is private with
     Default_Initial_Condition => Length (Set) = 0;

   function Length (Container : Set) return Count_Type;

   function Contains (Container : Set; Item : Element_Type) return Boolean;

   function Add (Container : Set; Item : Element_Type) return Set;

   function Remove (Container : Set; Item : Element_Type) return Set
     with Pre => Contains (Container, Item);

   function Union (Left, Right : Set) return Set;
   function Intersection (Left, Right : Set) return Set;
   function Difference (Left, Right : Set) return Set;

   function "=" (Left, Right : Set) return Boolean;
   function Is_Subset (Left, Right : Set) return Boolean;

   Empty_Set : constant Set;

private
   Max_Elements : constant := 100;

   type Element_Array is array (1 .. Max_Elements) of Element_Type;
   type Used_Array is array (1 .. Max_Elements) of Boolean;

   type Set is record
      Elements : Element_Array;
      Used     : Used_Array := (others => False);
      Count    : Count_Type := 0;
   end record;

   Empty_Set : constant Set := (Elements => <>, Used => (others => False), Count => 0);

end Ada.Containers.Functional_Sets;
