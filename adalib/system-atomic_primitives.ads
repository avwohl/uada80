-- System.Atomic_Primitives for Z80
-- Low-level atomic operations

package System.Atomic_Primitives is
   pragma Preelaborate;

   -- 8-bit atomic type (native Z80 size)
   type Atomic_8 is mod 2**8;
   pragma Atomic (Atomic_8);

   -- 16-bit atomic type (Z80 word size)
   type Atomic_16 is mod 2**16;
   pragma Atomic (Atomic_16);

   -- Lock-free atomic flag
   type Atomic_Flag is private;

   procedure Set (Flag : in Out Atomic_Flag);
   procedure Clear (Flag : in Out Atomic_Flag);
   function Test (Flag : Atomic_Flag) return Boolean;
   function Test_And_Set (Flag : in Out Atomic_Flag) return Boolean;

   -- Memory barriers (simplified for Z80)
   procedure Memory_Barrier;
   pragma Inline (Memory_Barrier);

   -- Compare and swap (uses DI/EI for atomicity)
   function Compare_And_Swap_8
     (Ptr      : System.Address;
      Expected : Atomic_8;
      Desired  : Atomic_8) return Boolean;

   function Compare_And_Swap_16
     (Ptr      : System.Address;
      Expected : Atomic_16;
      Desired  : Atomic_16) return Boolean;

private
   type Atomic_Flag is record
      Value : Boolean := False;
      pragma Atomic (Value);
   end record;

end System.Atomic_Primitives;
