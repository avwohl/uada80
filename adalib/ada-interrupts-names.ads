-- Ada.Interrupts.Names for Z80
-- Named interrupt constants for cpmemu emulator
--
-- The cpmemu emulator supports:
--   --mask-interrupt <range> <rst|call> <n>  (respects DI/EI)
--   --nmi <range>                            (always fires, jumps to 0x0066)
--
-- RST instructions jump to fixed addresses: RST n -> address n*8
-- CALL interrupts can target any address

package Ada.Interrupts.Names is
   pragma Preelaborate;

   -- Z80 RST instruction interrupts (fixed addresses)
   RST_00 : constant Interrupt_Id := 0;  -- 0x0000 - Reset (reserved)
   RST_08 : constant Interrupt_Id := 1;  -- 0x0008 - Available
   RST_10 : constant Interrupt_Id := 2;  -- 0x0010 - Available
   RST_18 : constant Interrupt_Id := 3;  -- 0x0018 - Available
   RST_20 : constant Interrupt_Id := 4;  -- 0x0020 - Available
   RST_28 : constant Interrupt_Id := 5;  -- 0x0028 - Available
   RST_30 : constant Interrupt_Id := 6;  -- 0x0030 - Available
   RST_38 : constant Interrupt_Id := 7;  -- 0x0038 - Mode 1 / CP/M (reserved)

   -- Aliases for common Z80 systems
   BDOS_Call    : constant Interrupt_Id := RST_38;
   System_Reset : constant Interrupt_Id := RST_00;

   -- Recommended for Ada tasking scheduler (use with --mask-interrupt)
   Timer_Tick   : constant Interrupt_Id := RST_10;  -- 0x0010

   -- NMI is special - always at 0x0066, cannot be disabled
   -- Use --nmi <range> on command line
   -- NMI is modeled separately, not as an Interrupt_Id

end Ada.Interrupts.Names;
