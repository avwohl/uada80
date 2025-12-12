-- System.Atomic_Operations.Exchange for Z80
-- Atomic exchange operations

generic
   type Atomic_Type is private;
package System.Atomic_Operations.Exchange
  with Pure
is
   function Atomic_Exchange
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type
     with Convention => Intrinsic;

   function Atomic_Compare_And_Exchange
     (Item    : aliased in Out Atomic_Type;
      Prior   : aliased in Out Atomic_Type;
      Desired : Atomic_Type) return Boolean
     with Convention => Intrinsic;

   function Is_Lock_Free return Boolean
     with Convention => Intrinsic;

end System.Atomic_Operations.Exchange;
