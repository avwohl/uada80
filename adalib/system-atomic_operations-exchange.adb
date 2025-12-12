-- System.Atomic_Operations.Exchange body for Z80
-- Atomic exchange operations implementation

package body System.Atomic_Operations.Exchange is

   ---------------------
   -- Atomic_Exchange --
   ---------------------

   function Atomic_Exchange
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type
   is
      Old_Value : Atomic_Type;
   begin
      -- On Z80, disable interrupts for atomicity
      -- DI instruction would go here in assembly
      Old_Value := Item;
      Item := Value;
      -- EI instruction would go here in assembly
      return Old_Value;
   end Atomic_Exchange;

   ---------------------------------
   -- Atomic_Compare_And_Exchange --
   ---------------------------------

   function Atomic_Compare_And_Exchange
     (Item    : aliased in Out Atomic_Type;
      Prior   : aliased in Out Atomic_Type;
      Desired : Atomic_Type) return Boolean
   is
   begin
      -- On Z80, disable interrupts for atomicity
      -- DI instruction would go here
      if Item = Prior then
         Item := Desired;
         -- EI instruction would go here
         return True;
      else
         Prior := Item;
         -- EI instruction would go here
         return False;
      end if;
   end Atomic_Compare_And_Exchange;

   ------------------
   -- Is_Lock_Free --
   ------------------

   function Is_Lock_Free return Boolean is
   begin
      -- Z80 single-threaded, so effectively lock-free
      return True;
   end Is_Lock_Free;

end System.Atomic_Operations.Exchange;
