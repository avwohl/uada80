-- System.Atomic_Primitives body for Z80
-- Atomic operations implementation

with System.Machine_Code;

package body System.Atomic_Primitives is

   ---------
   -- Set --
   ---------

   procedure Set (Flag : in Out Atomic_Flag) is
   begin
      System.Machine_Code.Asm ("di", Volatile => True);
      Flag.Value := True;
      System.Machine_Code.Asm ("ei", Volatile => True);
   end Set;

   -----------
   -- Clear --
   -----------

   procedure Clear (Flag : in Out Atomic_Flag) is
   begin
      System.Machine_Code.Asm ("di", Volatile => True);
      Flag.Value := False;
      System.Machine_Code.Asm ("ei", Volatile => True);
   end Clear;

   ----------
   -- Test --
   ----------

   function Test (Flag : Atomic_Flag) return Boolean is
   begin
      return Flag.Value;
   end Test;

   ------------------
   -- Test_And_Set --
   ------------------

   function Test_And_Set (Flag : in Out Atomic_Flag) return Boolean is
      Old_Value : Boolean;
   begin
      System.Machine_Code.Asm ("di", Volatile => True);
      Old_Value := Flag.Value;
      Flag.Value := True;
      System.Machine_Code.Asm ("ei", Volatile => True);
      return Old_Value;
   end Test_And_Set;

   --------------------
   -- Memory_Barrier --
   --------------------

   procedure Memory_Barrier is
   begin
      -- Z80 has no out-of-order execution, but we use DI/EI for safety
      System.Machine_Code.Asm ("di", Volatile => True);
      System.Machine_Code.Asm ("ei", Volatile => True);
   end Memory_Barrier;

   ------------------------
   -- Compare_And_Swap_8 --
   ------------------------

   function Compare_And_Swap_8
     (Ptr      : System.Address;
      Expected : Atomic_8;
      Desired  : Atomic_8) return Boolean
   is
      Mem : Atomic_8;
      for Mem'Address use Ptr;
      Result : Boolean;
   begin
      System.Machine_Code.Asm ("di", Volatile => True);
      if Mem = Expected then
         Mem := Desired;
         Result := True;
      else
         Result := False;
      end if;
      System.Machine_Code.Asm ("ei", Volatile => True);
      return Result;
   end Compare_And_Swap_8;

   -------------------------
   -- Compare_And_Swap_16 --
   -------------------------

   function Compare_And_Swap_16
     (Ptr      : System.Address;
      Expected : Atomic_16;
      Desired  : Atomic_16) return Boolean
   is
      Mem : Atomic_16;
      for Mem'Address use Ptr;
      Result : Boolean;
   begin
      System.Machine_Code.Asm ("di", Volatile => True);
      if Mem = Expected then
         Mem := Desired;
         Result := True;
      else
         Result := False;
      end if;
      System.Machine_Code.Asm ("ei", Volatile => True);
      return Result;
   end Compare_And_Swap_16;

end System.Atomic_Primitives;
