-- Ada.Containers.Functional_Vectors for Z80
-- Functional (immutable) vectors

generic
   type Index_Type is range <>;
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Functional_Vectors is
   pragma Pure;

   type Sequence is private with
     Default_Initial_Condition => Length (Sequence) = 0;

   function Length (Container : Sequence) return Count_Type;

   function Get (Container : Sequence; Index : Index_Type) return Element_Type
     with Pre => Index <= Index_Type'First + Index_Type'Base (Length (Container)) - 1;

   function Set
     (Container : Sequence;
      Index     : Index_Type;
      New_Item  : Element_Type) return Sequence
     with Pre => Index <= Index_Type'First + Index_Type'Base (Length (Container)) - 1;

   function Add (Container : Sequence; New_Item : Element_Type) return Sequence;

   function Remove (Container : Sequence; Index : Index_Type) return Sequence
     with Pre => Index <= Index_Type'First + Index_Type'Base (Length (Container)) - 1;

   function "=" (Left, Right : Sequence) return Boolean;

   Empty_Sequence : constant Sequence;

private
   Max_Elements : constant := 100;

   type Element_Array is array (1 .. Max_Elements) of Element_Type;

   type Sequence is record
      Data   : Element_Array;
      Length : Count_Type := 0;
   end record;

   Empty_Sequence : constant Sequence := (Data => <>, Length => 0);

end Ada.Containers.Functional_Vectors;
