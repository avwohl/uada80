-- System.String_Compare body for Z80
-- String comparison utilities implementation

with System.Storage_Elements;
use System.Storage_Elements;

package body System.String_Compare is

   -----------------
   -- Str_Compare --
   -----------------

   function Str_Compare
     (Left   : System.Address;
      Llen   : Natural;
      Right  : System.Address;
      Rlen   : Natural) return Integer
   is
      L : Storage_Array (1 .. Storage_Offset (Llen));
      for L'Address use Left;

      R : Storage_Array (1 .. Storage_Offset (Rlen));
      for R'Address use Right;

      Min_Len : constant Natural := Natural'Min (Llen, Rlen);
   begin
      for I in 1 .. Storage_Offset (Min_Len) loop
         if L (I) < R (I) then
            return -1;
         elsif L (I) > R (I) then
            return 1;
         end if;
      end loop;

      if Llen < Rlen then
         return -1;
      elsif Llen > Rlen then
         return 1;
      else
         return 0;
      end if;
   end Str_Compare;

   ---------------
   -- Str_Equal --
   ---------------

   function Str_Equal
     (Left   : System.Address;
      Llen   : Natural;
      Right  : System.Address;
      Rlen   : Natural) return Boolean
   is
   begin
      return Str_Compare (Left, Llen, Right, Rlen) = 0;
   end Str_Equal;

   --------------
   -- Str_Less --
   --------------

   function Str_Less
     (Left   : System.Address;
      Llen   : Natural;
      Right  : System.Address;
      Rlen   : Natural) return Boolean
   is
   begin
      return Str_Compare (Left, Llen, Right, Rlen) < 0;
   end Str_Less;

end System.String_Compare;
