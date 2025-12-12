-- GNAT.Debug_Utilities for Z80
-- Debug output utilities

package GNAT.Debug_Utilities is
   pragma Preelaborate;

   -- Image functions for debugging
   function Image (A : System.Address) return String;
   -- Returns hex representation of address

   function Image (N : Integer) return String;
   -- Returns decimal representation

   function Image (N : Long_Integer) return String;
   -- Returns decimal representation

end GNAT.Debug_Utilities;
