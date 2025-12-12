-- GNAT.Wide_String_Split for Z80
-- Wide string splitting utilities

package GNAT.Wide_String_Split is
   pragma Preelaborate;

   type Slice_Number is new Natural;
   type Slice_Set is private;

   type Separator_Mode is (Single, Multiple);

   procedure Create
     (S          : out Slice_Set;
      From       : Wide_String;
      Separators : Wide_String;
      Mode       : Separator_Mode := Single);
   --  Split From into slices at separators

   function Slice_Count (S : Slice_Set) return Slice_Number;
   --  Return number of slices

   function Slice
     (S     : Slice_Set;
      Index : Slice_Number) return Wide_String;
   --  Return the Index'th slice

private
   Max_Slices : constant := 100;
   Max_Length : constant := 1000;

   type Slice_Info is record
      First : Natural := 0;
      Last  : Natural := 0;
   end record;

   type Slice_Array is array (1 .. Max_Slices) of Slice_Info;
   type Wide_Char_Array is array (1 .. Max_Length) of Wide_Character;

   type Slice_Set is record
      Data       : Wide_Char_Array;
      Data_Last  : Natural := 0;
      Slices     : Slice_Array;
      Num_Slices : Slice_Number := 0;
   end record;

end GNAT.Wide_String_Split;
