-- GNAT.Perfect_Hash body for Z80
-- Perfect hash function generation implementation

package body GNAT.Perfect_Hash is

   package body Generator is

      --------------------------
      -- Compute_Perfect_Hash --
      --------------------------

      function Compute_Perfect_Hash
        (Key : Key_Type) return Hash_Type
      is
      begin
         -- Simple implementation - just use the regular hash
         return Hash (Key);
      end Compute_Perfect_Hash;

   end Generator;

end GNAT.Perfect_Hash;
