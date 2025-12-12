-- System.Fore for Z80
-- Fixed point Fore attribute computation

package System.Fore is
   pragma Pure;

   -- Compute Fore attribute for fixed point types
   function Fore_Fixed32
     (Lo, Hi : Long_Long_Integer;
      Scale  : Integer) return Natural;
   -- Lo, Hi are scaled values (value * 2**Scale)
   -- Returns minimum field width for integer part plus sign

   function Fore_Fixed64
     (Lo, Hi : Long_Long_Integer;
      Scale  : Integer) return Natural;
   -- For larger fixed point ranges

   -- Generic Fore computation
   generic
      type T is delta <>;
   function Generic_Fore return Natural;

end System.Fore;
