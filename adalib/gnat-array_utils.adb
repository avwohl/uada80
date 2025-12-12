-- GNAT.Array_Utils body for Z80
-- Generic array utilities implementation

package body GNAT.Array_Utils is

   --------------
   -- Find_Min --
   --------------

   function Find_Min (A : Array_Type) return Index_Type is
      Min_Idx : Index_Type := A'First;
   begin
      for I in A'Range loop
         if A (I) < A (Min_Idx) then
            Min_Idx := I;
         end if;
      end loop;
      return Min_Idx;
   end Find_Min;

   --------------
   -- Find_Max --
   --------------

   function Find_Max (A : Array_Type) return Index_Type is
      Max_Idx : Index_Type := A'First;
   begin
      for I in A'Range loop
         if A (Max_Idx) < A (I) then
            Max_Idx := I;
         end if;
      end loop;
      return Max_Idx;
   end Find_Max;

   -------------------
   -- Reverse_Array --
   -------------------

   procedure Reverse_Array (A : in Out Array_Type) is
      Temp : Element_Type;
      L    : Index_Type := A'First;
      R    : Index_Type := A'Last;
   begin
      while L < R loop
         Temp := A (L);
         A (L) := A (R);
         A (R) := Temp;
         L := Index_Type'Succ (L);
         R := Index_Type'Pred (R);
      end loop;
   end Reverse_Array;

   --------------
   -- Contains --
   --------------

   function Contains
     (A       : Array_Type;
      Element : Element_Type) return Boolean
   is
   begin
      for I in A'Range loop
         if A (I) = Element then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   --------------
   -- Index_Of --
   --------------

   function Index_Of
     (A       : Array_Type;
      Element : Element_Type) return Integer
   is
   begin
      for I in A'Range loop
         if A (I) = Element then
            return Index_Type'Pos (I);
         end if;
      end loop;
      return -1;
   end Index_Of;

end GNAT.Array_Utils;
