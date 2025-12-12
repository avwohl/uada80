-- System.Atomic_Operations body for Z80
-- Atomic operation implementation using DI/EI

package body System.Atomic_Operations is

   -- External assembly routines
   procedure Disable_Interrupts;
   pragma Import (Assembler, Disable_Interrupts, "_di");

   procedure Enable_Interrupts;
   pragma Import (Assembler, Enable_Interrupts, "_ei");

   -------------------------
   -- Atomic_Test_And_Set --
   -------------------------

   function Atomic_Test_And_Set
     (Item : aliased in Out Boolean) return Boolean
   is
      Old_Value : Boolean;
   begin
      Disable_Interrupts;
      Old_Value := Item;
      Item := True;
      Enable_Interrupts;
      return Old_Value;
   end Atomic_Test_And_Set;

   ------------------
   -- Atomic_Clear --
   ------------------

   procedure Atomic_Clear (Item : aliased in Out Boolean) is
   begin
      Disable_Interrupts;
      Item := False;
      Enable_Interrupts;
   end Atomic_Clear;

   -----------------
   -- Atomic_Load --
   -----------------

   function Atomic_Load (Item : aliased Atomic_Type) return Atomic_Type is
      Result : Atomic_Type;
   begin
      Disable_Interrupts;
      Result := Item;
      Enable_Interrupts;
      return Result;
   end Atomic_Load;

   ------------------
   -- Atomic_Store --
   ------------------

   procedure Atomic_Store
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type)
   is
   begin
      Disable_Interrupts;
      Item := Value;
      Enable_Interrupts;
   end Atomic_Store;

   ---------------------
   -- Atomic_Exchange --
   ---------------------

   function Atomic_Exchange
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type
   is
      Old_Value : Atomic_Type;
   begin
      Disable_Interrupts;
      Old_Value := Item;
      Item := Value;
      Enable_Interrupts;
      return Old_Value;
   end Atomic_Exchange;

   -----------------------------
   -- Atomic_Compare_Exchange --
   -----------------------------

   function Atomic_Compare_Exchange
     (Item     : aliased in Out Atomic_Type;
      Expected : aliased in Out Atomic_Type;
      Desired  : Atomic_Type) return Boolean
   is
      Success : Boolean;
   begin
      Disable_Interrupts;
      if Item = Expected then
         Item := Desired;
         Success := True;
      else
         Expected := Item;
         Success := False;
      end if;
      Enable_Interrupts;
      return Success;
   end Atomic_Compare_Exchange;

   ----------------
   -- Atomic_Add --
   ----------------

   function Atomic_Add
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type
   is
      New_Value : Atomic_Type;
   begin
      Disable_Interrupts;
      New_Value := Item + Value;
      Item := New_Value;
      Enable_Interrupts;
      return New_Value;
   end Atomic_Add;

   ---------------------
   -- Atomic_Subtract --
   ---------------------

   function Atomic_Subtract
     (Item  : aliased in Out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type
   is
      New_Value : Atomic_Type;
   begin
      Disable_Interrupts;
      New_Value := Item - Value;
      Item := New_Value;
      Enable_Interrupts;
      return New_Value;
   end Atomic_Subtract;

end System.Atomic_Operations;
