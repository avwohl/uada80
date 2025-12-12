-- Ada.Synchronous_Barriers body for Z80
-- Synchronous task barriers implementation

with System.Tasking;

package body Ada.Synchronous_Barriers is

   ----------------------
   -- Wait_For_Release --
   ----------------------

   procedure Wait_For_Release
     (The_Barrier : in Out Synchronous_Barrier;
      Notified    : out Boolean)
   is
   begin
      System.Tasking.Enter_Protected;

      The_Barrier.Count := The_Barrier.Count + 1;

      if The_Barrier.Count >= The_Barrier.Release_Threshold then
         -- Last task to arrive - reset and notify
         The_Barrier.Count := 0;
         Notified := True;
         System.Tasking.Leave_Protected;
         -- Other tasks will now be released when they check count
      else
         -- Not last task - wait for release
         Notified := False;
         System.Tasking.Leave_Protected;

         -- Spin until released (count resets to 0)
         loop
            System.Tasking.Enter_Protected;
            declare
               Released : constant Boolean := The_Barrier.Count = 0;
            begin
               System.Tasking.Leave_Protected;
               exit when Released;
            end;
            System.Tasking.Yield;
         end loop;
      end if;
   end Wait_For_Release;

end Ada.Synchronous_Barriers;
