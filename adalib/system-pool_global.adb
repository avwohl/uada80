-- System.Pool_Global body for Z80
-- Global unbounded storage pool implementation

with System.Memory;

package body System.Pool_Global is

   --------------
   -- Allocate --
   --------------

   overriding procedure Allocate
     (Pool                     : in Out Unbounded_No_Reclaim_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Pool, Alignment);
      use System.Storage_Elements;
   begin
      Storage_Address := System.Memory.Alloc (Integer (Size_In_Storage_Elements));
      if Storage_Address = System.Null_Address then
         raise Storage_Error;
      end if;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   overriding procedure Deallocate
     (Pool                     : in Out Unbounded_No_Reclaim_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Pool, Size_In_Storage_Elements, Alignment);
   begin
      System.Memory.Free (Storage_Address);
   end Deallocate;

   ------------------
   -- Storage_Size --
   ------------------

   overriding function Storage_Size
     (Pool : Unbounded_No_Reclaim_Pool)
      return System.Storage_Elements.Storage_Count
   is
      pragma Unreferenced (Pool);
   begin
      -- Return available heap memory on Z80
      return System.Storage_Elements.Storage_Count (System.Memory.Available);
   end Storage_Size;

end System.Pool_Global;
