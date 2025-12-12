-- Ada.Numerics.Float_Random for Z80
-- Float random number generator
--
-- Provides uniformly distributed random floating point numbers

with Ada.Numerics;

package Ada.Numerics.Float_Random is
   pragma Preelaborate;

   -- Generator state type
   type Generator is limited private;

   -- Uniformly distributed float type
   subtype Uniformly_Distributed is Float range 0.0 .. 1.0;

   -- Reset the generator
   procedure Reset (Gen : in Out Generator);
   -- Reset with implementation-defined seed

   procedure Reset (Gen : in Out Generator; Initiator : Integer);
   -- Reset with specified seed

   -- Generate random value
   function Random (Gen : in Out Generator) return Uniformly_Distributed;
   -- Returns a random value in [0.0, 1.0)

   -- Save/restore generator state
   type State is private;

   procedure Save (Gen : Generator; To_State : out State);
   procedure Reset (Gen : in Out Generator; From_State : State);

   function Image (Of_State : State) return String;
   function Value (Coded_State : String) return State;

   Max_Image_Width : constant := 20;

private

   type State is record
      Seed : Integer := 1;
   end record;

   type Generator is limited record
      Gen_State : State;
   end record;

end Ada.Numerics.Float_Random;
