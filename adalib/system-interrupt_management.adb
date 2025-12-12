-- System.Interrupt_Management body for Z80
-- Interrupt handling implementation

with System.Machine_Code;

package body System.Interrupt_Management is

   -- Interrupt handler table
   Handlers : array (Interrupt_ID) of Interrupt_Handler :=
     (others => null);

   -- Enabled flags
   Enabled_Flags : array (Interrupt_ID) of Boolean := (others => False);

   -- Global interrupt flag
   Global_Enabled : Boolean := False;

   --------------------
   -- Attach_Handler --
   --------------------

   procedure Attach_Handler
     (Handler   : Interrupt_Handler;
      Interrupt : Interrupt_ID)
   is
   begin
      System.Machine_Code.Asm ("di", Volatile => True);
      Handlers (Interrupt) := Handler;
      Enabled_Flags (Interrupt) := True;
      System.Machine_Code.Asm ("ei", Volatile => True);
   end Attach_Handler;

   --------------------
   -- Detach_Handler --
   --------------------

   procedure Detach_Handler (Interrupt : Interrupt_ID) is
   begin
      System.Machine_Code.Asm ("di", Volatile => True);
      Handlers (Interrupt) := null;
      Enabled_Flags (Interrupt) := False;
      System.Machine_Code.Asm ("ei", Volatile => True);
   end Detach_Handler;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt (Interrupt : Interrupt_ID) is
   begin
      Enabled_Flags (Interrupt) := True;
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt (Interrupt : Interrupt_ID) is
   begin
      Enabled_Flags (Interrupt) := False;
   end Disable_Interrupt;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts is
   begin
      Global_Enabled := True;
      System.Machine_Code.Asm ("ei", Volatile => True);
   end Enable_Interrupts;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts is
   begin
      System.Machine_Code.Asm ("di", Volatile => True);
      Global_Enabled := False;
   end Disable_Interrupts;

   ------------------------
   -- Interrupts_Enabled --
   ------------------------

   function Interrupts_Enabled return Boolean is
   begin
      return Global_Enabled;
   end Interrupts_Enabled;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      -- Set interrupt mode 1 (simple single handler at 0038h)
      System.Machine_Code.Asm ("im 1", Volatile => True);

      -- Enable interrupts
      Enable_Interrupts;
   end Initialize;

   -- Interrupt dispatch (called from assembly interrupt handler)
   procedure Dispatch_Interrupt (ID : Interrupt_ID) is
   begin
      if Handlers (ID) /= null and then Enabled_Flags (ID) then
         Handlers (ID).all;
      end if;
   end Dispatch_Interrupt;
   pragma Export (C, Dispatch_Interrupt, "ada_dispatch_interrupt");

end System.Interrupt_Management;
