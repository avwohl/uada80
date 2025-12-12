-- System.Random_Seed for Z80
-- Random seed generation

package System.Random_Seed is
   pragma Preelaborate;

   function Get_Seed return Integer;
   --  Get a seed value for random number generation
   --  On Z80, this uses the system tick counter

   procedure Set_Seed (Value : Integer);
   --  Set the global seed

end System.Random_Seed;
