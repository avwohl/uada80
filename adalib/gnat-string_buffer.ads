-- GNAT.String_Buffer for Z80
-- Efficient string buffer operations

package GNAT.String_Buffer is
   pragma Preelaborate;

   Default_Capacity : constant := 256;
   --  Default buffer capacity for Z80 (small due to memory constraints)

   type Buffer is limited private;
   --  A growable string buffer

   procedure Initialize (B : out Buffer; Capacity : Positive := Default_Capacity);
   --  Initialize buffer with given capacity

   procedure Finalize (B : in out Buffer);
   --  Release buffer resources

   procedure Append (B : in Out Buffer; S : String);
   --  Append string to buffer

   procedure Append (B : in Out Buffer; C : Character);
   --  Append character to buffer

   procedure Clear (B : in Out Buffer);
   --  Clear buffer contents

   function Length (B : Buffer) return Natural;
   --  Return current length

   function Capacity (B : Buffer) return Natural;
   --  Return current capacity

   function To_String (B : Buffer) return String;
   --  Return buffer contents as string

   function Element (B : Buffer; Index : Positive) return Character;
   --  Return character at Index

private

   Max_Buffer : constant := 4096;  -- Maximum buffer size for Z80

   type Buffer is record
      Data     : String (1 .. Max_Buffer);
      Length   : Natural := 0;
      Capacity : Natural := 0;
   end record;

end GNAT.String_Buffer;
