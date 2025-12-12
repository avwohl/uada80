-- System.Random_Numbers body for Z80
-- Low-level random number generation implementation

package body System.Random_Numbers is

   -- LCG parameters for 16-bit (good parameters for Z80)
   -- These give period of 32768
   A : constant := 21;
   C : constant := 1;
   M : constant := 32768;

   -----------
   -- Reset --
   -----------

   procedure Reset (G : out Generator) is
   begin
      G.Current := 1;
   end Reset;

   procedure Reset (G : out Generator; Seed : Integer) is
   begin
      G.Current := State (abs (Seed) mod M);
      if G.Current = 0 then
         G.Current := 1;
      end if;
   end Reset;

   ----------
   -- Next --
   ----------

   procedure Next (G : in Out Generator) is
   begin
      G.Current := State ((Integer (G.Current) * A + C) mod M);
   end Next;

   ------------
   -- Random --
   ------------

   function Random (G : Generator) return Float is
   begin
      return Float (G.Current) / Float (M - 1);
   end Random;

   function Random (G : Generator) return Integer is
   begin
      return Integer (G.Current);
   end Random;

   ----------
   -- Save --
   ----------

   function Save (G : Generator) return State is
   begin
      return G.Current;
   end Save;

   -------------
   -- Restore --
   -------------

   procedure Restore (G : out Generator; S : State) is
   begin
      G.Current := S;
   end Restore;

end System.Random_Numbers;
