-- Ada.Execution_Time.Interrupts body for Z80
-- Interrupt execution time tracking implementation

package body Ada.Execution_Time.Interrupts is

   -- Per-interrupt CPU time (8 RST vectors)
   Interrupt_Times : array (0 .. 7) of Ada.Real_Time.Time :=
     (others => Ada.Real_Time.Time_First);

   -----------
   -- Clock --
   -----------

   function Clock (Interrupt : Ada.Interrupts.Interrupt_Id)
     return CPU_Time
   is
      Index : constant Natural := Natural (Interrupt) / 8;
      -- RST vectors are at 0x00, 0x08, 0x10, etc.
   begin
      if Index in Interrupt_Times'Range then
         return CPU_Time (Interrupt_Times (Index));
      else
         return CPU_Time_First;
      end if;
   end Clock;

   ---------------
   -- Supported --
   ---------------

   function Supported (Interrupt : Ada.Interrupts.Interrupt_Id)
     return Boolean
   is
      Index : constant Natural := Natural (Interrupt) / 8;
   begin
      return Index in Interrupt_Times'Range;
   end Supported;

   -- Internal: Add time to an interrupt
   procedure Add_Interrupt_Time
     (Interrupt : Ada.Interrupts.Interrupt_Id;
      Amount    : Ada.Real_Time.Time_Span)
   is
      Index : constant Natural := Natural (Interrupt) / 8;
   begin
      if Index in Interrupt_Times'Range then
         Interrupt_Times (Index) := Interrupt_Times (Index) + Amount;
      end if;
   end Add_Interrupt_Time;

end Ada.Execution_Time.Interrupts;
