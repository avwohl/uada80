-- System.Memop for Z80
-- Low-level memory operations

with System.Storage_Elements;

package System.Memop is
   pragma Pure;

   use System.Storage_Elements;

   -- Copy memory from source to destination
   procedure Memcpy
     (Dest   : System.Address;
      Source : System.Address;
      Size   : Storage_Count);

   -- Move memory (handles overlapping regions)
   procedure Memmove
     (Dest   : System.Address;
      Source : System.Address;
      Size   : Storage_Count);

   -- Fill memory with a byte value
   procedure Memset
     (Dest  : System.Address;
      Value : Storage_Element;
      Size  : Storage_Count);

   -- Compare memory regions
   function Memcmp
     (Left  : System.Address;
      Right : System.Address;
      Size  : Storage_Count) return Integer;

   -- Search for byte in memory
   function Memchr
     (Source : System.Address;
      Value  : Storage_Element;
      Size   : Storage_Count) return System.Address;
   -- Returns Null_Address if not found

end System.Memop;
