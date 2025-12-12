-- Ada.Containers.Prime_Numbers for Z80
-- Prime number utilities for hash tables

package Ada.Containers.Prime_Numbers is
   pragma Pure;

   function To_Prime (Size : Count_Type) return Count_Type;
   --  Return smallest prime >= Size

   function Is_Prime (N : Count_Type) return Boolean;
   --  Check if N is prime

end Ada.Containers.Prime_Numbers;
