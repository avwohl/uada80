-- GNAT.MBBS_Float_Random body for Z80
-- Mersenne Twister based float random implementation

package body GNAT.MBBS_Float_Random is

   ------------
   -- Random --
   ------------

   function Random (Gen : Generator) return Uniformly_Distributed is
      Temp : Natural;
   begin
      -- Update state (simplified LCG)
      Temp := (Gen.State * 1103515245 + 12345) mod 65536;

      -- Convert to float in [0, 1)
      return Uniformly_Distributed (Float (Temp) / 65536.0);
   end Random;

   -----------
   -- Reset --
   -----------

   procedure Reset (Gen : in Out Generator) is
   begin
      Gen.State := 54321;
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Gen : in Out Generator; Initiator : Integer) is
   begin
      Gen.State := abs (Initiator) mod 65536;
      if Gen.State = 0 then
         Gen.State := 1;
      end if;
   end Reset;

   -----------
   -- Image --
   -----------

   function Image (Of_State : Generator) return String is
      S : constant String := Natural'Image (Of_State.State);
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Coded_State : String) return Generator is
      Result : Generator;
   begin
      Result.State := Natural'Value (Coded_State);
      return Result;
   exception
      when others =>
         Result.State := 12345;
         return Result;
   end Value;

end GNAT.MBBS_Float_Random;
