-- Ada.Numerics.Discrete_Random body for Z80
-- Discrete random number generator implementation

package body Ada.Numerics.Discrete_Random is

   -- LCG parameters for 16-bit
   A : constant := 25173;
   C : constant := 13849;
   M : constant := 65536;

   -- Internal: advance the generator
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
      -- Use a fixed default seed
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

   function Random (Gen : in Out Generator) return Result_Subtype is
      Range_Size : Integer;
      Raw_Value  : Integer;
   begin
      -- Advance the generator
      Advance (Gen);

      -- Scale to result range
      Range_Size := Result_Subtype'Pos (Result_Subtype'Last) -
                    Result_Subtype'Pos (Result_Subtype'First) + 1;

      Raw_Value := Gen.Gen_State.Seed mod Range_Size;

      return Result_Subtype'Val (
        Result_Subtype'Pos (Result_Subtype'First) + Raw_Value);
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
      Neg    : Boolean := Value < 0;
   begin
      if Neg then
         Value := -Value;
      end if;

      -- Convert to string (simple implementation)
      for I in reverse Result'Range loop
         Result (I) := Character'Val (Character'Pos ('0') + (Value mod 10));
         Value := Value / 10;
         Idx := I;
         exit when Value = 0;
      end loop;

      if Neg then
         Idx := Idx - 1;
         Result (Idx) := '-';
      end if;

      return Result (Idx .. Result'Last);
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Coded_State : String) return State is
      Result : State;
      Num    : Integer := 0;
      Neg    : Boolean := False;
      Start  : Positive := Coded_State'First;
   begin
      -- Skip leading spaces
      while Start <= Coded_State'Last and then Coded_State (Start) = ' ' loop
         Start := Start + 1;
      end loop;

      -- Check for negative
      if Start <= Coded_State'Last and then Coded_State (Start) = '-' then
         Neg := True;
         Start := Start + 1;
      end if;

      -- Parse digits
      for I in Start .. Coded_State'Last loop
         exit when Coded_State (I) < '0' or Coded_State (I) > '9';
         Num := Num * 10 + (Character'Pos (Coded_State (I)) - Character'Pos ('0'));
      end loop;

      if Neg then
         Num := -Num;
      end if;

      Result.Seed := Num mod M;
      return Result;
   end Value;

end Ada.Numerics.Discrete_Random;
