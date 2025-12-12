-- GNAT.Sort_Utils for Z80
-- Generic sorting utilities optimized for Z80 memory constraints

package GNAT.Sort_Utils is
   pragma Pure;

   -- Integer array sorting (non-generic for common case)
   type Int_Array is array (Positive range <>) of Integer;

   -- Sort integer array in place (insertion sort - good for small arrays)
   procedure Sort_Integers (Data : in Out Int_Array);

   -- Sort integer array descending
   procedure Sort_Integers_Desc (Data : in Out Int_Array);

   -- Check if array is sorted
   function Is_Sorted (Data : Int_Array) return Boolean;
   function Is_Sorted_Desc (Data : Int_Array) return Boolean;

   -- Find minimum/maximum position
   function Min_Index (Data : Int_Array) return Positive;
   function Max_Index (Data : Int_Array) return Positive;

   -- Binary search (requires sorted array)
   function Binary_Search (Data : Int_Array; Value : Integer) return Natural;
   -- Returns 0 if not found, otherwise index of element

   -- Linear search
   function Linear_Search (Data : Int_Array; Value : Integer) return Natural;

   -- Reverse array in place
   procedure Reverse_Array (Data : in Out Int_Array);

   -- Rotate array left by N positions
   procedure Rotate_Left (Data : in Out Int_Array; N : Natural);

   -- Rotate array right by N positions
   procedure Rotate_Right (Data : in Out Int_Array; N : Natural);

   -- Swap two elements
   procedure Swap (Data : in Out Int_Array; I, J : Positive);

   -- Shuffle array (requires random seed to be set)
   procedure Shuffle (Data : in Out Int_Array; Seed : in Out Natural);

   -- Partition around pivot (returns pivot position)
   function Partition (Data : in Out Int_Array;
                       Low, High : Positive) return Positive;

   -- Selection: find kth smallest element
   function Kth_Smallest (Data : in Out Int_Array;
                          K : Positive) return Integer;

   -- Remove duplicates (array must be sorted first)
   -- Returns new length
   function Remove_Duplicates (Data : in Out Int_Array) return Natural;

   -- Merge two sorted arrays into result
   procedure Merge_Sorted (A, B : Int_Array;
                           Result : out Int_Array;
                           Length : out Natural);

   -- Copy portion of array
   procedure Copy (Source : Int_Array;
                   Dest : out Int_Array;
                   Count : Natural);

   -- Fill array with value
   procedure Fill (Data : out Int_Array; Value : Integer);

   -- Fill array with sequence (start, start+step, start+2*step, ...)
   procedure Fill_Sequence (Data : out Int_Array;
                            Start : Integer := 1;
                            Step : Integer := 1);

   -- String array sorting
   Max_Str_Len : constant := 32;
   Max_Str_Count : constant := 16;

   type Fixed_String is record
      Data : String (1 .. Max_Str_Len);
      Len  : Natural;
   end record;

   type Str_Array is array (Positive range <>) of Fixed_String;

   -- Sort string array alphabetically
   procedure Sort_Strings (Data : in Out Str_Array);

   -- Sort strings case-insensitive
   procedure Sort_Strings_CI (Data : in Out Str_Array);

   -- Compare strings for sorting
   function String_Less (A, B : Fixed_String) return Boolean;
   function String_Less_CI (A, B : Fixed_String) return Boolean;

   -- Create Fixed_String from String
   function To_Fixed_String (S : String) return Fixed_String;

   -- Extract String from Fixed_String
   function To_String (FS : Fixed_String) return String;

end GNAT.Sort_Utils;
