-- Ada.Containers.Functional_Vectors body for Z80
-- Functional vectors implementation

package body Ada.Containers.Functional_Vectors is

   ------------
   -- Length --
   ------------

   function Length (Container : Sequence) return Count_Type is
   begin
      return Container.Length;
   end Length;

   ---------
   -- Get --
   ---------

   function Get (Container : Sequence; Index : Index_Type) return Element_Type is
      Idx : constant Positive := Positive (Index - Index_Type'First + 1);
   begin
      return Container.Data (Idx);
   end Get;

   ---------
   -- Set --
   ---------

   function Set
     (Container : Sequence;
      Index     : Index_Type;
      New_Item  : Element_Type) return Sequence
   is
      Result : Sequence := Container;
      Idx    : constant Positive := Positive (Index - Index_Type'First + 1);
   begin
      Result.Data (Idx) := New_Item;
      return Result;
   end Set;

   ---------
   -- Add --
   ---------

   function Add (Container : Sequence; New_Item : Element_Type) return Sequence is
      Result : Sequence := Container;
   begin
      if Result.Length < Count_Type (Max_Elements) then
         Result.Length := Result.Length + 1;
         Result.Data (Positive (Result.Length)) := New_Item;
      end if;
      return Result;
   end Add;

   ------------
   -- Remove --
   ------------

   function Remove (Container : Sequence; Index : Index_Type) return Sequence is
      Result : Sequence;
      Idx    : constant Positive := Positive (Index - Index_Type'First + 1);
      J      : Positive := 1;
   begin
      for I in 1 .. Positive (Container.Length) loop
         if I /= Idx then
            Result.Data (J) := Container.Data (I);
            J := J + 1;
         end if;
      end loop;
      Result.Length := Container.Length - 1;
      return Result;
   end Remove;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Sequence) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      for I in 1 .. Positive (Left.Length) loop
         if Left.Data (I) /= Right.Data (I) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

end Ada.Containers.Functional_Vectors;
