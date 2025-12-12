-- GNAT.Perfect_Hash for Z80
-- Perfect hash function generation support

package GNAT.Perfect_Hash is
   pragma Pure;

   type Hash_Type is mod 2**16;
   --  Hash value type for Z80

   generic
      type Key_Type (<>) is private;
      with function Hash (Key : Key_Type) return Hash_Type;
      with function Equal (Left, Right : Key_Type) return Boolean;
   package Generator is

      function Compute_Perfect_Hash
        (Key : Key_Type) return Hash_Type;
      --  Compute hash assuming perfect function exists

   end Generator;

end GNAT.Perfect_Hash;
