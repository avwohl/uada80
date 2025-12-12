-- System.Random_Seed body for Z80
-- Random seed generation implementation

package body System.Random_Seed is

   Current_Seed : Integer := 12345;

   --------------
   -- Get_Seed --
   --------------

   function Get_Seed return Integer is
   begin
      -- Simple LFSR to advance seed
      Current_Seed := Current_Seed * 1103515245 + 12345;
      return Current_Seed;
   end Get_Seed;

   --------------
   -- Set_Seed --
   --------------

   procedure Set_Seed (Value : Integer) is
   begin
      if Value = 0 then
         Current_Seed := 1;
      else
         Current_Seed := Value;
      end if;
   end Set_Seed;

end System.Random_Seed;
