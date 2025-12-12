-- System.Storage_Pools for Z80
-- Storage pool abstraction
--
-- Provides the root type for user-defined storage pools

with System.Storage_Elements;

package System.Storage_Pools is
   pragma Preelaborate;

   type Root_Storage_Pool is abstract tagged limited private;
   pragma Preelaborable_Initialization (Root_Storage_Pool);

   -- Allocate storage from the pool
   procedure Allocate
     (Pool                     : in Out Root_Storage_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is abstract;

   -- Deallocate storage from the pool
   procedure Deallocate
     (Pool                     : in Out Root_Storage_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is abstract;

   -- Return the total storage size of the pool
   function Storage_Size
     (Pool : Root_Storage_Pool)
      return System.Storage_Elements.Storage_Count
   is abstract;

private

   type Root_Storage_Pool is abstract tagged limited null record;

end System.Storage_Pools;
