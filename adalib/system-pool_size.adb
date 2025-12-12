-- System.Pool_Size body for Z80
-- Bounded storage pool implementation

package body System.Pool_Size is

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Pool         : in Out Stack_Bounded_Pool;
      Address      : out System.Address;
      Storage_Size : Storage_Count;
      Alignment    : Storage_Count)
   is
      pragma Unreferenced (Alignment);
      Aligned_Size : Storage_Count;
   begin
      -- Round up to element size
      if Pool.Elmt_Size > 0 then
         Aligned_Size := Pool.Elmt_Size;
      else
         Aligned_Size := Storage_Size;
      end if;

      if Pool.First_Free + Aligned_Size <= Pool.Pool_Size then
         Address := Pool.Storage (Pool.First_Free + 1)'Address;
         Pool.First_Free := Pool.First_Free + Aligned_Size;
      else
         raise Storage_Error;
      end if;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (Pool         : in Out Stack_Bounded_Pool;
      Address      : System.Address;
      Storage_Size : Storage_Count;
      Alignment    : Storage_Count)
   is
      pragma Unreferenced (Pool, Address, Storage_Size, Alignment);
   begin
      -- Stack-based pool doesn't support individual deallocation
      -- Memory is reclaimed when pool is destroyed
      null;
   end Deallocate;

   ------------------
   -- Storage_Size --
   ------------------

   function Storage_Size
     (Pool : Stack_Bounded_Pool) return Storage_Count
   is
   begin
      return Pool.Pool_Size;
   end Storage_Size;

end System.Pool_Size;
