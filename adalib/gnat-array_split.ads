-- GNAT.Array_Split for Z80
-- Array splitting utilities

generic
   type Element is private;
   type Element_Sequence is array (Positive range <>) of Element;
   type Element_Set is private;
   with function Is_In (E : Element; S : Element_Set) return Boolean;
package GNAT.Array_Split is
   pragma Preelaborate;

   type Slice_Number is new Natural;
   type Slice_Set is private;

   type Separator_Mode is (Single, Multiple);

   procedure Create
     (S          : out Slice_Set;
      From       : Element_Sequence;
      Separators : Element_Set;
      Mode       : Separator_Mode := Single);
   --  Split From into slices at separators

   function Slice_Count (S : Slice_Set) return Slice_Number;
   --  Return number of slices

   function Slice
     (S     : Slice_Set;
      Index : Slice_Number) return Element_Sequence;
   --  Return the Index'th slice

   type Slice_Separators is array (Positive range <>) of Element;

   function Separators
     (S     : Slice_Set;
      Index : Slice_Number) return Slice_Separators;
   --  Return separators before slice Index

private
   Max_Slices : constant := 100;

   type Slice_Info is record
      First : Natural := 0;
      Last  : Natural := 0;
   end record;

   type Slice_Array is array (1 .. Max_Slices) of Slice_Info;
   type Element_Array is array (1 .. 1000) of Element;

   type Slice_Set is record
      Data       : Element_Array;
      Data_Last  : Natural := 0;
      Slices     : Slice_Array;
      Num_Slices : Slice_Number := 0;
   end record;

end GNAT.Array_Split;
