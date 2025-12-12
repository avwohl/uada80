-- System.Atomic_Operations.Integer_Arithmetic for Z80
-- Atomic integer arithmetic operations

generic
   type Atomic_Type is range <>;
package System.Atomic_Operations.Integer_Arithmetic
  with Pure
is
   function Atomic_Fetch_And_Add
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type
     with Convention => Intrinsic;

   function Atomic_Fetch_And_Subtract
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type
     with Convention => Intrinsic;

   procedure Atomic_Add
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type)
     with Convention => Intrinsic;

   procedure Atomic_Subtract
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type)
     with Convention => Intrinsic;

   function Is_Lock_Free return Boolean
     with Convention => Intrinsic;

end System.Atomic_Operations.Integer_Arithmetic;
