-- GNAT.Signals body for Z80
-- Unix signal handling stubs

package body GNAT.Signals is

   -- Signal handlers table
   Handlers : array (Signal range 1 .. 32) of Handler := (others => null);
   Blocked  : array (Signal range 1 .. 32) of Boolean := (others => False);

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Handler
     (Sig     : Signal;
      Handler : GNAT.Signals.Handler)
   is
   begin
      if Sig in Handlers'Range then
         Handlers (Sig) := Handler;
      end if;
   end Install_Handler;

   ------------------
   -- Raise_Signal --
   ------------------

   procedure Raise_Signal (Sig : Signal) is
   begin
      if Sig in Handlers'Range and then
         not Blocked (Sig) and then
         Handlers (Sig) /= null
      then
         Handlers (Sig) (Sig);
      end if;
   end Raise_Signal;

   ------------------
   -- Block_Signal --
   ------------------

   procedure Block_Signal (Sig : Signal) is
   begin
      if Sig in Blocked'Range then
         Blocked (Sig) := True;
      end if;
   end Block_Signal;

   --------------------
   -- Unblock_Signal --
   --------------------

   procedure Unblock_Signal (Sig : Signal) is
   begin
      if Sig in Blocked'Range then
         Blocked (Sig) := False;
      end if;
   end Unblock_Signal;

end GNAT.Signals;
