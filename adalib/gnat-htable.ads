-- GNAT.HTable for Z80
-- Generic hash table

package GNAT.HTable is
   pragma Pure;

   -- Simple hash table
   generic
      type Header_Num is range <>;
      -- Range of hash values

      type Element is limited private;
      -- Element type

      type Elmt_Ptr is private;
      -- Pointer to element

      Null_Ptr : Elmt_Ptr;
      -- Null pointer value

      with function Next (E : Elmt_Ptr) return Elmt_Ptr;
      with procedure Set_Next (E : Elmt_Ptr; Next : Elmt_Ptr);
      with function Get_Key (E : Elmt_Ptr) return String;
      with function Hash (F : String) return Header_Num;
      with function Equal (F1, F2 : String) return Boolean;

   package Static_HTable is
      procedure Reset;
      procedure Set (E : Elmt_Ptr);
      function Get (K : String) return Elmt_Ptr;
      procedure Remove (K : String);
      function Get_First return Elmt_Ptr;
      function Get_Next return Elmt_Ptr;
   end Static_HTable;

   -- Simple hash function
   function Hash (Key : String) return Natural;

end GNAT.HTable;
