-- System.Pool_Global for Z80
-- Global unbounded storage pool
--
-- Provides a global heap-based storage pool

with System.Storage_Pools;
with System.Storage_Elements;

package System.Pool_Global is
   pragma Preelaborate;

   -- Unbounded global storage pool
   type Unbounded_No_Reclaim_Pool is new
     System.Storage_Pools.Root_Storage_Pool with private;

   -- Standard pool operations
   overriding procedure Allocate
     (Pool                     : in Out Unbounded_No_Reclaim_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count);

   overriding procedure Deallocate
     (Pool                     : in Out Unbounded_No_Reclaim_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count);

   overriding function Storage_Size
     (Pool : Unbounded_No_Reclaim_Pool)
      return System.Storage_Elements.Storage_Count;

   -- Global pool instance
   Global_Pool_Object : Unbounded_No_Reclaim_Pool;

private

   type Unbounded_No_Reclaim_Pool is new
     System.Storage_Pools.Root_Storage_Pool with null record;

end System.Pool_Global;
