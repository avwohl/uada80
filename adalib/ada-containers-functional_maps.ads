-- Ada.Containers.Functional_Maps for Z80
-- Functional (immutable) maps

generic
   type Key_Type is private;
   type Element_Type is private;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Functional_Maps is
   pragma Pure;

   type Map is private with
     Default_Initial_Condition => Length (Map) = 0;

   function Length (Container : Map) return Count_Type;

   function Has_Key (Container : Map; Key : Key_Type) return Boolean;

   function Get (Container : Map; Key : Key_Type) return Element_Type
     with Pre => Has_Key (Container, Key);

   function Set
     (Container : Map;
      Key       : Key_Type;
      New_Item  : Element_Type) return Map;

   function Add
     (Container : Map;
      Key       : Key_Type;
      New_Item  : Element_Type) return Map
     with Pre => not Has_Key (Container, Key);

   function Remove (Container : Map; Key : Key_Type) return Map
     with Pre => Has_Key (Container, Key);

   function "=" (Left, Right : Map) return Boolean;

   Empty_Map : constant Map;

private
   Max_Elements : constant := 50;

   type Entry_Type is record
      Key   : Key_Type;
      Value : Element_Type;
      Used  : Boolean := False;
   end record;

   type Entry_Array is array (1 .. Max_Elements) of Entry_Type;

   type Map is record
      Entries : Entry_Array;
      Count   : Count_Type := 0;
   end record;

   Empty_Map : constant Map := (Entries => <>, Count => 0);

end Ada.Containers.Functional_Maps;
