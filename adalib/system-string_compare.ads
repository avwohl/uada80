-- System.String_Compare for Z80
-- String comparison utilities

package System.String_Compare is
   pragma Pure;

   function Str_Compare
     (Left   : System.Address;
      Llen   : Natural;
      Right  : System.Address;
      Rlen   : Natural) return Integer;
   --  Compare two strings, return <0, 0, or >0

   function Str_Equal
     (Left   : System.Address;
      Llen   : Natural;
      Right  : System.Address;
      Rlen   : Natural) return Boolean;
   --  Check if two strings are equal

   function Str_Less
     (Left   : System.Address;
      Llen   : Natural;
      Right  : System.Address;
      Rlen   : Natural) return Boolean;
   --  Check if Left < Right

end System.String_Compare;
