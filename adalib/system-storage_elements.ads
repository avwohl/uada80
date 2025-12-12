-- System.Storage_Elements for Z80
-- Low-level storage manipulation
--
-- Re-exports and extends System storage types

with System;

package System.Storage_Elements is
   pragma Pure;

   -- Re-export types from System
   type Storage_Offset is range -(2**15) .. (2**15) - 1;
   subtype Storage_Count is Storage_Offset range 0 .. Storage_Offset'Last;

   type Storage_Element is mod 2**8;
   for Storage_Element'Size use 8;

   type Storage_Array is array (Storage_Offset range <>) of aliased Storage_Element;
   for Storage_Array'Component_Size use 8;

   -- Address is an integer type
   type Integer_Address is mod 2**16;

   -- Conversion functions
   function To_Address (Value : Integer_Address) return System.Address;
   function To_Integer (Value : System.Address) return Integer_Address;

   -- Address arithmetic (re-export)
   function "+" (Left : System.Address; Right : Storage_Offset) return System.Address
     renames System."+";
   function "+" (Left : Storage_Offset; Right : System.Address) return System.Address
     renames System."+";
   function "-" (Left : System.Address; Right : Storage_Offset) return System.Address
     renames System."-";
   function "-" (Left, Right : System.Address) return Storage_Offset
     renames System."-";

   function "mod" (Left : System.Address; Right : Storage_Offset) return Storage_Offset
     renames System."mod";

end System.Storage_Elements;
