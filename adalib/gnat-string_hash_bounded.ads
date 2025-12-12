-- GNAT.String_Hash_Bounded for Z80
-- Hash functions for bounded strings

with Ada.Strings.Bounded;
with Ada.Containers;

generic
   with package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length (<>);
package GNAT.String_Hash_Bounded is
   pragma Preelaborate;

   function Hash (Key : Bounded.Bounded_String) return Ada.Containers.Hash_Type;
   --  Compute hash value for bounded string

end GNAT.String_Hash_Bounded;
