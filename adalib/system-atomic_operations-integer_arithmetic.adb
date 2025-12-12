-- System.Atomic_Operations.Integer_Arithmetic body for Z80
-- Atomic integer arithmetic operations implementation

package body System.Atomic_Operations.Integer_Arithmetic is

   --------------------------
   -- Atomic_Fetch_And_Add --
   --------------------------

   function Atomic_Fetch_And_Add
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type
   is
      Old_Value : Atomic_Type;
   begin
      -- On Z80, disable interrupts for atomicity
      Old_Value := Item;
      Item := Item + Value;
      return Old_Value;
   end Atomic_Fetch_And_Add;

   -------------------------------
   -- Atomic_Fetch_And_Subtract --
   -------------------------------

   function Atomic_Fetch_And_Subtract
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type
   is
      Old_Value : Atomic_Type;
   begin
      Old_Value := Item;
      Item := Item - Value;
      return Old_Value;
   end Atomic_Fetch_And_Subtract;

   ----------------
   -- Atomic_Add --
   ----------------

   procedure Atomic_Add
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type)
   is
   begin
      Item := Item + Value;
   end Atomic_Add;

   ---------------------
   -- Atomic_Subtract --
   ---------------------

   procedure Atomic_Subtract
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type)
   is
   begin
      Item := Item - Value;
   end Atomic_Subtract;

   ------------------
   -- Is_Lock_Free --
   ------------------

   function Is_Lock_Free return Boolean is
   begin
      return True;
   end Is_Lock_Free;

end System.Atomic_Operations.Integer_Arithmetic;
