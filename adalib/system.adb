-- System package body for Z80
-- Address arithmetic implementations

package body System is

   function "+" (Left : Address; Right : Storage_Offset) return Address is
   begin
      return Address ((Integer (Left) + Integer (Right)) mod Memory_Size);
   end "+";

   function "+" (Left : Storage_Offset; Right : Address) return Address is
   begin
      return Right + Left;
   end "+";

   function "-" (Left : Address; Right : Storage_Offset) return Address is
   begin
      return Address ((Integer (Left) - Integer (Right)) mod Memory_Size);
   end "-";

   function "-" (Left : Address; Right : Address) return Storage_Offset is
   begin
      return Storage_Offset (Integer (Left) - Integer (Right));
   end "-";

   function "mod" (Left : Address; Right : Storage_Offset) return Storage_Offset is
   begin
      return Storage_Offset (Integer (Left) mod Integer (Right));
   end "mod";

end System;
