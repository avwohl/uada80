-- System.Atomic_Operations for Z80
-- Atomic operation support
--
-- Note: Z80 is single-threaded but DI/EI can be used for interrupt safety.
-- These operations disable interrupts during the operation.

package System.Atomic_Operations is
   pragma Preelaborate;

   -- Atomic test and set
   function Atomic_Test_And_Set
     (Item : aliased in Out Boolean) return Boolean;
   -- Sets Item to True and returns the previous value

   -- Atomic clear
   procedure Atomic_Clear (Item : aliased in Out Boolean);
   -- Sets Item to False

   -- Atomic load
   generic
      type Atomic_Type is private;
   function Atomic_Load (Item : aliased Atomic_Type) return Atomic_Type;
   -- Atomically reads Item

   -- Atomic store
   generic
      type Atomic_Type is private;
   procedure Atomic_Store
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type);
   -- Atomically writes Value to Item

   -- Atomic exchange
   generic
      type Atomic_Type is private;
   function Atomic_Exchange
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type;
   -- Atomically exchanges Item with Value, returns old value

   -- Atomic compare and exchange
   generic
      type Atomic_Type is private;
   function Atomic_Compare_Exchange
     (Item     : aliased in Out Atomic_Type;
      Expected : aliased in Out Atomic_Type;
      Desired  : Atomic_Type) return Boolean;
   -- If Item = Expected, sets Item := Desired and returns True
   -- Otherwise sets Expected := Item and returns False

   -- Atomic add
   generic
      type Atomic_Type is range <>;
   function Atomic_Add
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type;
   -- Atomically adds Value to Item, returns new value

   -- Atomic subtract
   generic
      type Atomic_Type is range <>;
   function Atomic_Subtract
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type;
   -- Atomically subtracts Value from Item, returns new value

end System.Atomic_Operations;
