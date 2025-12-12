-- System.Pool_Local for Z80
-- Local bounded storage pool
--
-- Provides a stack-based bounded storage pool

with System.Storage_Pools;
with System.Storage_Elements;

package System.Pool_Local is
   pragma Preelaborate;

   -- Bounded local storage pool
   type Unbounded_Reclaim_Pool is new
     System.Storage_Pools.Root_Storage_Pool with private;

   -- Standard pool operations
   overriding procedure Allocate
     (Pool                     : in Out Unbounded_Reclaim_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count);

   overriding procedure Deallocate
     (Pool                     : in Out Unbounded_Reclaim_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count);

   overriding function Storage_Size
     (Pool : Unbounded_Reclaim_Pool)
      return System.Storage_Elements.Storage_Count;

private

   type Unbounded_Reclaim_Pool is new
     System.Storage_Pools.Root_Storage_Pool with null record;

end System.Pool_Local;
