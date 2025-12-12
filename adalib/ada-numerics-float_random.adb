-- Ada.Numerics.Float_Random body for Z80
-- Float random number generator implementation

package body Ada.Numerics.Float_Random is

   -- LCG parameters for 16-bit
   A : constant := 25173;
   C : constant := 13849;
   M : constant := 65536;

   procedure Advance (Gen : in Out Generator) is
      New_Seed : Integer;
   begin
      New_Seed := (A * Gen.Gen_State.Seed + C) mod M;
      Gen.Gen_State.Seed := New_Seed;
   end Advance;

   -----------
   -- Reset --
   -----------

   procedure Reset (Gen : in Out Generator) is
   begin
      Gen.Gen_State.Seed := 12345;
   end Reset;

   procedure Reset (Gen : in Out Generator; Initiator : Integer) is
   begin
      Gen.Gen_State.Seed := Initiator mod M;
      if Gen.Gen_State.Seed = 0 then
         Gen.Gen_State.Seed := 1;
      end if;
   end Reset;

   ------------
   -- Random --
   ------------

   function Random (Gen : in Out Generator) return Uniformly_Distributed is
   begin
      Advance (Gen);
      -- Convert integer [0, M-1] to float [0.0, 1.0)
      return Float (Gen.Gen_State.Seed) / Float (M);
   end Random;

   ----------
   -- Save --
   ----------

   procedure Save (Gen : Generator; To_State : out State) is
   begin
      To_State := Gen.Gen_State;
   end Save;

   procedure Reset (Gen : in Out Generator; From_State : State) is
   begin
      Gen.Gen_State := From_State;
   end Reset;

   -----------
   -- Image --
   -----------

   function Image (Of_State : State) return String is
      Result : String (1 .. 10);
      Value  : Integer := Of_State.Seed;
      Idx    : Natural := 10;
   begin
      for I in reverse Result'Range loop
         Result (I) := Character'Val (Character'Pos ('0') + (Value mod 10));
         Value := Value / 10;
         Idx := I;
         exit when Value = 0;
      end loop;

      return Result (Idx .. Result'Last);
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Coded_State : String) return State is
      Result : State;
      Num    : Integer := 0;
      Start  : Positive := Coded_State'First;
   begin
      while Start <= Coded_State'Last and then Coded_State (Start) = ' ' loop
         Start := Start + 1;
      end loop;

      for I in Start .. Coded_State'Last loop
         exit when Coded_State (I) < '0' or Coded_State (I) > '9';
         Num := Num * 10 + (Character'Pos (Coded_State (I)) - Character'Pos ('0'));
      end loop;

      Result.Seed := Num mod M;
      return Result;
   end Value;

end Ada.Numerics.Float_Random;
