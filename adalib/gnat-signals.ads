-- GNAT.Signals for Z80
-- Unix signal handling (stub for CP/M)

package GNAT.Signals is

   -- Signal types (Unix-style numbers)
   type Signal is new Integer;

   SIGHUP    : constant Signal := 1;
   SIGINT    : constant Signal := 2;
   SIGQUIT   : constant Signal := 3;
   SIGILL    : constant Signal := 4;
   SIGTRAP   : constant Signal := 5;
   SIGABRT   : constant Signal := 6;
   SIGFPE    : constant Signal := 8;
   SIGKILL   : constant Signal := 9;
   SIGBUS    : constant Signal := 10;
   SIGSEGV   : constant Signal := 11;
   SIGPIPE   : constant Signal := 13;
   SIGALRM   : constant Signal := 14;
   SIGTERM   : constant Signal := 15;
   SIGUSR1   : constant Signal := 16;
   SIGUSR2   : constant Signal := 17;
   SIGCHLD   : constant Signal := 18;

   -- Signal handler type
   type Handler is access procedure (Sig : Signal);
   pragma Favor_Top_Level (Handler);

   -- Signal actions
   SIG_DFL : constant Handler := null;
   SIG_IGN : constant Handler := null;

   -- Install signal handler
   procedure Install_Handler
     (Sig     : Signal;
      Handler : GNAT.Signals.Handler);

   -- Raise signal
   procedure Raise_Signal (Sig : Signal);

   -- Block/unblock signals
   procedure Block_Signal (Sig : Signal);
   procedure Unblock_Signal (Sig : Signal);

end GNAT.Signals;
