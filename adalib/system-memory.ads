-- System.Memory for Z80
-- Low-level memory allocation

with System.Storage_Elements;

package System.Memory is
   pragma Preelaborate;

   -- Memory allocation
   function Alloc (Size : System.Storage_Elements.Storage_Count) return Address;
   pragma Export (C, Alloc, "__gnat_malloc");

   procedure Free (Addr : Address);
   pragma Export (C, Free, "__gnat_free");

   function Realloc
     (Addr : Address;
      Size : System.Storage_Elements.Storage_Count) return Address;
   pragma Export (C, Realloc, "__gnat_realloc");

   -- Memory info
   function Available return System.Storage_Elements.Storage_Count;

end System.Memory;
