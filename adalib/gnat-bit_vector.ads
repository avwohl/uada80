-- GNAT.Bit_Vector for Z80
-- Dynamic bit vector with bounded storage

package GNAT.Bit_Vector is
   pragma Pure;

   Max_Bits : constant := 256;  -- Maximum bits for Z80

   type Bit_Vector is private;

   function Create (Size : Positive) return Bit_Vector;
   --  Create bit vector of given size

   function Size (V : Bit_Vector) return Natural;
   --  Return number of bits

   function Get (V : Bit_Vector; Index : Natural) return Boolean;
   --  Get bit at index (0-based)

   procedure Set (V : in Out Bit_Vector; Index : Natural; Value : Boolean := True);
   --  Set bit at index

   procedure Clear (V : in Out Bit_Vector; Index : Natural);
   --  Clear bit at index (set to False)

   procedure Toggle (V : in Out Bit_Vector; Index : Natural);
   --  Toggle bit at index

   procedure Set_All (V : out Bit_Vector);
   --  Set all bits to True

   procedure Clear_All (V : out Bit_Vector);
   --  Clear all bits to False

   procedure Fill (V : out Bit_Vector; Value : Boolean);
   --  Set all bits to given value

   function Count (V : Bit_Vector) return Natural;
   --  Count number of set bits (population count)

   function Any (V : Bit_Vector) return Boolean;
   --  Return True if any bit is set

   function All_Set (V : Bit_Vector) return Boolean;
   --  Return True if all bits are set

   function None (V : Bit_Vector) return Boolean;
   --  Return True if no bits are set

   function First_Set (V : Bit_Vector) return Integer;
   --  Return index of first set bit (-1 if none)

   function Last_Set (V : Bit_Vector) return Integer;
   --  Return index of last set bit (-1 if none)

   function First_Clear (V : Bit_Vector) return Integer;
   --  Return index of first clear bit (-1 if none)

   -- Bitwise operations
   function "and" (Left, Right : Bit_Vector) return Bit_Vector;
   function "or" (Left, Right : Bit_Vector) return Bit_Vector;
   function "xor" (Left, Right : Bit_Vector) return Bit_Vector;
   function "not" (V : Bit_Vector) return Bit_Vector;

   function "=" (Left, Right : Bit_Vector) return Boolean;

   -- Subset/superset tests
   function Is_Subset (V, Of_Set : Bit_Vector) return Boolean;
   function Is_Superset (V, Of_Set : Bit_Vector) return Boolean;
   function Intersects (Left, Right : Bit_Vector) return Boolean;

   -- Shift operations
   procedure Shift_Left (V : in Out Bit_Vector; Count : Natural);
   procedure Shift_Right (V : in Out Bit_Vector; Count : Natural);
   procedure Rotate_Left (V : in Out Bit_Vector; Count : Natural);
   procedure Rotate_Right (V : in Out Bit_Vector; Count : Natural);

   -- Range operations
   procedure Set_Range (V : in Out Bit_Vector; Low, High : Natural);
   procedure Clear_Range (V : in Out Bit_Vector; Low, High : Natural);

   -- Conversion
   function To_Natural (V : Bit_Vector) return Natural;
   --  Convert first 16 bits to Natural (for Z80)

   function From_Natural (Value : Natural; Size : Positive) return Bit_Vector;
   --  Create from Natural value

private

   Max_Bytes : constant := Max_Bits / 8;

   type Byte_Array is array (1 .. Max_Bytes) of Natural range 0 .. 255;

   type Bit_Vector is record
      Data : Byte_Array := (others => 0);
      Bits : Natural := 0;
   end record;

end GNAT.Bit_Vector;
