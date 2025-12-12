-- GNAT.Sparse_Array for Z80
-- Sparse array with integer indices and values

package GNAT.Sparse_Array is
   pragma Pure;

   Max_Entries : constant := 64;  -- Maximum non-zero entries for Z80

   type Sparse_Array is private;

   procedure Initialize (A : out Sparse_Array);
   --  Initialize empty sparse array

   function Count (A : Sparse_Array) return Natural;
   --  Return number of non-zero entries

   function Get (A : Sparse_Array; Index : Integer) return Integer;
   --  Get value at index (0 if not set)

   procedure Set (A : in Out Sparse_Array; Index : Integer; Value : Integer);
   --  Set value at index (removes if Value = 0)

   function Contains (A : Sparse_Array; Index : Integer) return Boolean;
   --  Check if index has non-zero value

   procedure Remove (A : in Out Sparse_Array; Index : Integer);
   --  Remove entry at index

   procedure Clear (A : out Sparse_Array);
   --  Clear all entries

   function Min_Index (A : Sparse_Array) return Integer;
   --  Return minimum index with value (0 if empty)

   function Max_Index (A : Sparse_Array) return Integer;
   --  Return maximum index with value (0 if empty)

   -- Arithmetic operations (element-wise)
   function Add (A, B : Sparse_Array) return Sparse_Array;
   function Subtract (A, B : Sparse_Array) return Sparse_Array;
   function Scale (A : Sparse_Array; S : Integer) return Sparse_Array;

   -- Dot product
   function Dot_Product (A, B : Sparse_Array) return Integer;

   -- Sum of all values
   function Sum (A : Sparse_Array) return Integer;

   -- Iteration support
   type Entry_Info is record
      Index : Integer;
      Value : Integer;
      Valid : Boolean;
   end record;

   function Get_Entry (A : Sparse_Array; N : Positive) return Entry_Info;
   --  Get Nth entry (1-based)

private

   type Entry_Type is record
      Index : Integer := 0;
      Value : Integer := 0;
      Valid : Boolean := False;
   end record;

   type Entry_Array is array (1 .. Max_Entries) of Entry_Type;

   type Sparse_Array is record
      Entries : Entry_Array;
      Count   : Natural := 0;
   end record;

end GNAT.Sparse_Array;
