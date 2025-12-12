-- Ada.Numerics.Discrete_Random for Z80
-- Discrete random number generator
--
-- Provides random numbers for discrete types

with Ada.Numerics;

generic
   type Result_Subtype is (<>);
package Ada.Numerics.Discrete_Random is
   pragma Preelaborate;

   -- Generator state type
   type Generator is limited private;

   -- Reset the generator
   procedure Reset (Gen : in Out Generator);
   -- Reset with implementation-defined seed

   procedure Reset (Gen : in Out Generator; Initiator : Integer);
   -- Reset with specified seed

   -- Generate random value
   function Random (Gen : in Out Generator) return Result_Subtype;
   -- Returns a random value in Result_Subtype'Range

   -- Save/restore generator state
   type State is private;

   procedure Save (Gen : Generator; To_State : out State);
   procedure Reset (Gen : in Out Generator; From_State : State);

   function Image (Of_State : State) return String;
   function Value (Coded_State : String) return State;

   -- Maximum image width
   Max_Image_Width : constant := 20;

private

   -- Linear congruential generator state
   -- Using parameters suitable for 16-bit arithmetic:
   -- X(n+1) = (A * X(n) + C) mod M
   -- A = 25173, C = 13849, M = 65536

   type State is record
      Seed : Integer := 1;
   end record;

   type Generator is limited record
      Gen_State : State;
   end record;

end Ada.Numerics.Discrete_Random;
