-- GNAT.Random body for Z80
-- Simple random number generation implementation

package body GNAT.Random is

   -----------
   -- Reset --
   -----------

   procedure Reset (Gen : out Generator) is
   begin
      Gen.State := 12345;
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Gen : out Generator; Seed : Integer) is
   begin
      if Seed = 0 then
         Gen.State := 1;
      else
         Gen.State := Seed;
      end if;
   end Reset;

   ------------
   -- Random --
   ------------

   function Random (Gen : Generator) return Float is
      S   : Integer := Gen.State;
      Bit : Integer;
   begin
      -- Simple LFSR
      Bit := ((S / 1) + (S / 4) + (S / 32) + (S / 64)) mod 2;
      S := (S / 2) + Bit * 16384;
      if S = 0 then
         S := 1;
      end if;
      return Float (S mod 10000) / 10000.0;
   end Random;

   --------------------
   -- Random_Integer --
   --------------------

   function Random_Integer
     (Gen  : Generator;
      Low  : Integer;
      High : Integer) return Integer
   is
      R : constant Float := Random (Gen);
   begin
      return Low + Integer (R * Float (High - Low + 1)) mod (High - Low + 1);
   end Random_Integer;

   --------------------
   -- Random_Boolean --
   --------------------

   function Random_Boolean (Gen : Generator) return Boolean is
   begin
      return Random (Gen) >= 0.5;
   end Random_Boolean;

end GNAT.Random;
