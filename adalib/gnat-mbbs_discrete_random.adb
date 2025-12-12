-- GNAT.MBBS_Discrete_Random body for Z80
-- Mersenne Twister based discrete random implementation

package body GNAT.MBBS_Discrete_Random is

   -- Linear congruential generator (simplified for Z80)
   LCG_A : constant := 1103515245;
   LCG_C : constant := 12345;
   LCG_M : constant := 2147483648;

   ------------
   -- Random --
   ------------

   function Random (Gen : Generator) return Result_Subtype is
      Temp  : Natural;
      Range_Size : constant Natural :=
        Result_Subtype'Pos (Result_Subtype'Last) -
        Result_Subtype'Pos (Result_Subtype'First) + 1;
      Index : Natural;
   begin
      -- Update state (simplified LCG)
      Temp := (Gen.State * 1103515245 + 12345) mod 65536;

      -- Scale to result range
      Index := Temp mod Range_Size;

      return Result_Subtype'Val
        (Result_Subtype'Pos (Result_Subtype'First) + Index);
   end Random;

   -----------
   -- Reset --
   -----------

   procedure Reset (Gen : in Out Generator) is
   begin
      -- Use a simple default seed (time not available on basic Z80)
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
      return S (S'First + 1 .. S'Last);  -- Remove leading space
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

end GNAT.MBBS_Discrete_Random;
