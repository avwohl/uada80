-- System.Random_Numbers for Z80
-- Low-level random number generation
--
-- Provides a simple linear congruential generator suitable for Z80

package System.Random_Numbers is
   pragma Preelaborate;

   type Generator is private;

   -- Initialize the generator with a seed
   procedure Reset (G : out Generator);
   -- Reset with default seed

   procedure Reset (G : out Generator; Seed : Integer);
   -- Reset with specified seed

   -- Get random values
   function Random (G : Generator) return Float;
   -- Returns value in range 0.0 .. 1.0

   function Random (G : Generator) return Integer;
   -- Returns value in full Integer range

   -- Advance the generator state
   procedure Next (G : in out Generator);

   -- Save and restore state
   type State is private;

   function Save (G : Generator) return State;
   procedure Restore (G : out Generator; S : State);

private
   -- Linear congruential generator state
   -- Using parameters suitable for 16-bit arithmetic
   type State is new Integer range 0 .. 32767;

   type Generator is record
      Current : State := 1;
   end record;

end System.Random_Numbers;
