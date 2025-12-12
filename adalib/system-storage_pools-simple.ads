-- System.Storage_Pools.Simple for Z80
-- Simple storage pool implementation

with System.Storage_Elements;

package System.Storage_Pools.Simple is
   pragma Preelaborate;

   Pool_Size : constant := 2048;  -- 2KB pool for Z80

   type Simple_Pool is new Root_Storage_Pool with private;

   overriding procedure Allocate
     (Pool                     : in Out Simple_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count);

   overriding procedure Deallocate
     (Pool                     : in Out Simple_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count);

   overriding function Storage_Size
     (Pool : Simple_Pool) return System.Storage_Elements.Storage_Count;

   procedure Initialize (Pool : out Simple_Pool);
   --  Initialize pool

   function Available (Pool : Simple_Pool) return Natural;
   --  Return available bytes

   function Used (Pool : Simple_Pool) return Natural;
   --  Return used bytes

   function Allocation_Count (Pool : Simple_Pool) return Natural;
   --  Return number of allocations

private

   type Pool_Data is array (1 .. Pool_Size) of
     System.Storage_Elements.Storage_Element;

   type Simple_Pool is new Root_Storage_Pool with record
      Data       : Pool_Data;
      Next_Free  : Natural := 1;  -- Next free position
      Alloc_Count : Natural := 0;
   end record;

end System.Storage_Pools.Simple;
