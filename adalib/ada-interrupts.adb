-- Ada.Interrupts body for Z80
-- Interrupt handling support implementation

package body Ada.Interrupts is

   -- Z80 RST vector addresses: 0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38
   RST_Vectors : constant array (Interrupt_Id) of System.Address :=
     (0 => System.Address (16#0000#),
      1 => System.Address (16#0008#),
      2 => System.Address (16#0010#),
      3 => System.Address (16#0018#),
      4 => System.Address (16#0020#),
      5 => System.Address (16#0028#),
      6 => System.Address (16#0030#),
      7 => System.Address (16#0038#));

   -- Handler table
   Handlers : array (Interrupt_Id) of Parameterless_Handler := (others => null);

   -- Reserved interrupts (RST 0 is reset, RST 7 is used by CP/M)
   Reserved : constant array (Interrupt_Id) of Boolean :=
     (0 => True, 7 => True, others => False);

   -----------------
   -- Is_Reserved --
   -----------------

   function Is_Reserved (Interrupt : Interrupt_Id) return Boolean is
   begin
      return Reserved (Interrupt);
   end Is_Reserved;

   -----------------
   -- Is_Attached --
   -----------------

   function Is_Attached (Interrupt : Interrupt_Id) return Boolean is
   begin
      return Handlers (Interrupt) /= null;
   end Is_Attached;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler (Interrupt : Interrupt_Id) return Parameterless_Handler is
   begin
      return Handlers (Interrupt);
   end Current_Handler;

   --------------------
   -- Attach_Handler --
   --------------------

   procedure Attach_Handler
     (New_Handler : Parameterless_Handler;
      Interrupt   : Interrupt_Id)
   is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;
      Handlers (Interrupt) := New_Handler;
   end Attach_Handler;

   ----------------------
   -- Exchange_Handler --
   ----------------------

   procedure Exchange_Handler
     (Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt   : Interrupt_Id)
   is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;
      Old_Handler := Handlers (Interrupt);
      Handlers (Interrupt) := New_Handler;
   end Exchange_Handler;

   --------------------
   -- Detach_Handler --
   --------------------

   procedure Detach_Handler (Interrupt : Interrupt_Id) is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;
      Handlers (Interrupt) := null;
   end Detach_Handler;

   ---------------
   -- Reference --
   ---------------

   function Reference (Interrupt : Interrupt_Id) return System.Address is
   begin
      return RST_Vectors (Interrupt);
   end Reference;

end Ada.Interrupts;
