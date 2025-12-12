-- Interfaces.C.Extensions for Z80
-- C extensions (GNAT specific)

package Interfaces.C.Extensions is
   pragma Pure;

   -- Extended C types
   type long_long is range -2**63 .. 2**63 - 1;
   type unsigned_long_long is mod 2**64;

   -- Boolean type (C99)
   type bool is new Boolean;
   pragma Convention (C, bool);

   -- Size types
   subtype ssize_t is ptrdiff_t;

   -- Void pointer
   subtype void_ptr is System.Address;

   -- Constants
   Null_Ptr : constant void_ptr := System.Null_Address;

end Interfaces.C.Extensions;
