-- Ada.Synchronous_Barriers for Z80
-- Synchronous task barriers (Ada 2012)

package Ada.Synchronous_Barriers is
   pragma Preelaborate;

   subtype Barrier_Limit is Positive range 1 .. 8;
   -- Max 8 tasks can wait at a barrier (matches Max_Tasks)

   type Synchronous_Barrier (Release_Threshold : Barrier_Limit) is
      limited private;

   -- Wait at barrier
   -- Notified is True for exactly one task (the last to arrive)
   procedure Wait_For_Release
     (The_Barrier : in out Synchronous_Barrier;
      Notified    : out Boolean);

private

   type Synchronous_Barrier (Release_Threshold : Barrier_Limit) is
      limited record
         Count : Natural := 0;
      end record;

end Ada.Synchronous_Barriers;
