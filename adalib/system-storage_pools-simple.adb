-- System.Storage_Pools.Simple body for Z80
-- Simple storage pool implementation (bump allocator)

package body System.Storage_Pools.Simple is

   use System.Storage_Elements;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Pool : out Simple_Pool) is
   begin
      Pool.Data := (others => 0);
      Pool.Next_Free := 1;
      Pool.Alloc_Count := 0;
   end Initialize;

   --------------
   -- Allocate --
   --------------

   overriding procedure Allocate
     (Pool                     : in Out Simple_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Alignment);
      Size : constant Natural := Natural (Size_In_Storage_Elements);
      Addr : Natural;
   begin
      -- Simple bump allocation
      if Pool.Next_Free + Size - 1 <= Pool_Size then
         Addr := Pool.Next_Free;
         Pool.Next_Free := Pool.Next_Free + Size;
         Pool.Alloc_Count := Pool.Alloc_Count + 1;
         Storage_Address := Pool.Data (Addr)'Address;
      else
         -- Pool exhausted
         Storage_Address := System.Null_Address;
         raise Storage_Error with "Simple_Pool exhausted";
      end if;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   overriding procedure Deallocate
     (Pool                     : in Out Simple_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Storage_Address, Size_In_Storage_Elements, Alignment);
   begin
      -- Simple pool does not support deallocation
      -- (bump allocator pattern)
      null;
   end Deallocate;

   ------------------
   -- Storage_Size --
   ------------------

   overriding function Storage_Size
     (Pool : Simple_Pool) return System.Storage_Elements.Storage_Count
   is
      pragma Unreferenced (Pool);
   begin
      return Storage_Count (Pool_Size);
   end Storage_Size;

   ---------------
   -- Available --
   ---------------

   function Available (Pool : Simple_Pool) return Natural is
   begin
      return Pool_Size - Pool.Next_Free + 1;
   end Available;

   ----------
   -- Used --
   ----------

   function Used (Pool : Simple_Pool) return Natural is
   begin
      return Pool.Next_Free - 1;
   end Used;

   ----------------------
   -- Allocation_Count --
   ----------------------

   function Allocation_Count (Pool : Simple_Pool) return Natural is
   begin
      return Pool.Alloc_Count;
   end Allocation_Count;

end System.Storage_Pools.Simple;
