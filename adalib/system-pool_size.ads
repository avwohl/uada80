-- System.Pool_Size for Z80
-- Bounded storage pool implementation

with System.Storage_Elements;
with System.Storage_Pools;

package System.Pool_Size is

   use System.Storage_Elements;

   -- Stack-based bounded pool
   type Stack_Bounded_Pool
     (Pool_Size : Storage_Count;
      Elmt_Size : Storage_Count;
      Alignment : Storage_Count) is
     new System.Storage_Pools.Root_Storage_Pool with private;

   procedure Allocate
     (Pool         : in Out Stack_Bounded_Pool;
      Address      : out System.Address;
      Storage_Size : Storage_Count;
      Alignment    : Storage_Count);

   procedure Deallocate
     (Pool         : in Out Stack_Bounded_Pool;
      Address      : System.Address;
      Storage_Size : Storage_Count;
      Alignment    : Storage_Count);

   function Storage_Size
     (Pool : Stack_Bounded_Pool) return Storage_Count;

private
   type Stack_Bounded_Pool
     (Pool_Size : Storage_Count;
      Elmt_Size : Storage_Count;
      Alignment : Storage_Count) is
     new System.Storage_Pools.Root_Storage_Pool with record
      First_Free : Storage_Count := 0;
      Storage    : Storage_Array (1 .. Pool_Size);
   end record;

end System.Pool_Size;
