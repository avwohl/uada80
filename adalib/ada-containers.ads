-- Ada.Containers root package for Z80
-- Defines types and exceptions for container library

package Ada.Containers is
   pragma Pure;

   -- Count type for container lengths
   type Count_Type is range 0 .. 65535;

   -- Hash type for hashed containers
   type Hash_Type is mod 2**16;

   -- Standard container exceptions
   Capacity_Error : exception;

end Ada.Containers;
