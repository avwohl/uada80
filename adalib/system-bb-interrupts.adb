-- System.BB.Interrupts body for Z80
-- Bare board interrupt handling implementation

package body System.BB.Interrupts is

   -- Handler table (8 RST vectors + NMI)
   Handlers : array (0 .. 8) of Interrupt_Handler := (others => null);

   -- Enabled flags
   Enabled : array (0 .. 8) of Boolean := (others => False);

   -- Map vector address to index
   function Vector_To_Index (Vector : Natural) return Natural is
   begin
      case Vector is
         when RST_00 => return 0;
         when RST_08 => return 1;
         when RST_10 => return 2;
         when RST_18 => return 3;
         when RST_20 => return 4;
         when RST_28 => return 5;
         when RST_30 => return 6;
         when RST_38 => return 7;
         when NMI    => return 8;
         when others => return 0;
      end case;
   end Vector_To_Index;

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Handler
     (Vector  : Natural;
      Handler : Interrupt_Handler)
   is
      Index : constant Natural := Vector_To_Index (Vector);
   begin
      if Index <= Handlers'Last then
         Handlers (Index) := Handler;
      end if;
   end Install_Handler;

   --------------------
   -- Remove_Handler --
   --------------------

   procedure Remove_Handler (Vector : Natural) is
      Index : constant Natural := Vector_To_Index (Vector);
   begin
      if Index <= Handlers'Last then
         Handlers (Index) := null;
      end if;
   end Remove_Handler;

   -----------------
   -- Get_Handler --
   -----------------

   function Get_Handler (Vector : Natural) return Interrupt_Handler is
      Index : constant Natural := Vector_To_Index (Vector);
   begin
      if Index <= Handlers'Last then
         return Handlers (Index);
      else
         return null;
      end if;
   end Get_Handler;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt (Vector : Natural) is
      Index : constant Natural := Vector_To_Index (Vector);
   begin
      if Index <= Enabled'Last then
         Enabled (Index) := True;
      end if;
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt (Vector : Natural) is
      Index : constant Natural := Vector_To_Index (Vector);
   begin
      if Index <= Enabled'Last then
         Enabled (Index) := False;
      end if;
   end Disable_Interrupt;

   ----------------
   -- Is_Enabled --
   ----------------

   function Is_Enabled (Vector : Natural) return Boolean is
      Index : constant Natural := Vector_To_Index (Vector);
   begin
      if Index <= Enabled'Last then
         return Enabled (Index);
      else
         return False;
      end if;
   end Is_Enabled;

   -- Generic interrupt dispatcher (called from assembly stubs)
   procedure Dispatch_Interrupt (Index : Natural) is
   begin
      if Index <= Handlers'Last and then Handlers (Index) /= null then
         Handlers (Index).all;
      end if;
   end Dispatch_Interrupt;
   pragma Export (C, Dispatch_Interrupt, "ada_dispatch_int");

end System.BB.Interrupts;
