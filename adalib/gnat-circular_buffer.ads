-- GNAT.Circular_Buffer for Z80
-- Fixed-size circular buffer implementation

package GNAT.Circular_Buffer is
   pragma Pure;

   Max_Capacity : constant := 256;  -- Maximum buffer size for Z80

   type Buffer is private;

   function Create (Capacity : Positive) return Buffer;
   --  Create buffer with specified capacity (up to Max_Capacity)

   function Is_Empty (B : Buffer) return Boolean;
   --  Check if buffer is empty

   function Is_Full (B : Buffer) return Boolean;
   --  Check if buffer is full

   function Length (B : Buffer) return Natural;
   --  Return number of elements in buffer

   function Capacity (B : Buffer) return Positive;
   --  Return maximum capacity

   function Available (B : Buffer) return Natural;
   --  Return available space

   procedure Put (B : in Out Buffer; Item : Character);
   --  Add character to buffer (overwrites oldest if full)

   procedure Put (B : in Out Buffer; Data : String);
   --  Add string to buffer

   function Get (B : in Out Buffer) return Character;
   --  Get and remove oldest character (returns NUL if empty)

   procedure Get (B : in Out Buffer; Data : out String; Last : out Natural);
   --  Get up to Data'Length characters

   function Peek (B : Buffer) return Character;
   --  Peek at oldest character without removing

   function Peek (B : Buffer; Index : Positive) return Character;
   --  Peek at character at index (1 = oldest)

   procedure Clear (B : out Buffer);
   --  Clear all contents

   procedure Discard (B : in Out Buffer; Count : Natural);
   --  Discard oldest Count characters

   -- Byte buffer variant
   type Byte_Buffer is private;

   function Create_Byte (Capacity : Positive) return Byte_Buffer;

   function Is_Empty (B : Byte_Buffer) return Boolean;
   function Is_Full (B : Byte_Buffer) return Boolean;
   function Length (B : Byte_Buffer) return Natural;
   function Capacity (B : Byte_Buffer) return Positive;

   procedure Put (B : in Out Byte_Buffer; Item : Natural);
   function Get (B : in Out Byte_Buffer) return Natural;
   function Peek (B : Byte_Buffer) return Natural;
   procedure Clear (B : out Byte_Buffer);

private

   type Char_Array is array (1 .. Max_Capacity) of Character;
   type Byte_Array is array (1 .. Max_Capacity) of Natural range 0 .. 255;

   type Buffer is record
      Data   : Char_Array := (others => ASCII.NUL);
      Head   : Positive := 1;  -- Next read position
      Tail   : Positive := 1;  -- Next write position
      Count  : Natural := 0;   -- Current count
      Cap    : Positive := Max_Capacity;
   end record;

   type Byte_Buffer is record
      Data   : Byte_Array := (others => 0);
      Head   : Positive := 1;
      Tail   : Positive := 1;
      Count  : Natural := 0;
      Cap    : Positive := Max_Capacity;
   end record;

end GNAT.Circular_Buffer;
