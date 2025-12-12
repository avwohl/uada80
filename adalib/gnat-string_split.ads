-- GNAT.String_Split for Z80
-- String splitting utilities

package GNAT.String_Split is
   pragma Preelaborate;

   type Slice_Set is private;
   type Slice_Number is new Natural;
   type Separator_Mode is (Single, Multiple);

   -- Create slice set by splitting a string
   procedure Create
     (S          : out Slice_Set;
      From       : String;
      Separators : String;
      Mode       : Separator_Mode := Single);

   -- Get number of slices
   function Slice_Count (S : Slice_Set) return Slice_Number;

   -- Get a specific slice
   function Slice (S : Slice_Set; Index : Slice_Number) return String;

   -- Iterate over separators
   type Separators_Callback is access procedure (Sep : String);

private

   Max_Slices : constant := 16;

   type Slice_Info is record
      First : Natural := 0;
      Last  : Natural := 0;
   end record;

   type Slice_Array is array (1 .. Max_Slices) of Slice_Info;

   type Slice_Set is record
      Source : String (1 .. 256);
      Length : Natural := 0;
      Slices : Slice_Array;
      Count  : Slice_Number := 0;
   end record;

end GNAT.String_Split;
