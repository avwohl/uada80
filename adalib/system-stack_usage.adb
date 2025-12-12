-- System.Stack_Usage body for Z80
-- Stack usage tracking implementation

with Ada.Text_IO;
with System.Storage_Elements;

package body System.Stack_Usage is

   use System.Storage_Elements;

   Fill_Pattern : constant Storage_Element := 16#A5#;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Stats : out Stack_Stats;
      Base  : System.Address;
      Size  : Natural)
   is
   begin
      Stats.Stack_Base := Base;
      Stats.Stack_Size := Size;
      Stats.Peak_Usage := 0;
      Stats.Current_Used := 0;
   end Initialize;

   ------------
   -- Update --
   ------------

   procedure Update (Stats : in Out Stack_Stats; Current_SP : System.Address) is
      use type System.Address;
      Used : Natural;
   begin
      if Current_SP < Stats.Stack_Base then
         Used := Natural (Stats.Stack_Base - Current_SP);
         if Used > Stats.Stack_Size then
            Used := Stats.Stack_Size;  -- Cap at max
         end if;
      else
         Used := 0;
      end if;

      Stats.Current_Used := Used;
      if Used > Stats.Peak_Usage then
         Stats.Peak_Usage := Used;
      end if;
   end Update;

   ------------
   -- Report --
   ------------

   procedure Report (Stats : Stack_Stats) is
   begin
      Ada.Text_IO.Put ("Stack Size: ");
      Ada.Text_IO.Put_Line (Natural'Image (Stats.Stack_Size));
      Ada.Text_IO.Put ("Peak Usage: ");
      Ada.Text_IO.Put_Line (Natural'Image (Stats.Peak_Usage));
      Ada.Text_IO.Put ("Current:    ");
      Ada.Text_IO.Put_Line (Natural'Image (Stats.Current_Used));
   end Report;

   ----------------
   -- Fill_Stack --
   ----------------

   procedure Fill_Stack (Base : System.Address; Size : Natural) is
      Stack : Storage_Array (1 .. Storage_Offset (Size));
      for Stack'Address use Base;
   begin
      Stack := (others => Fill_Pattern);
   end Fill_Stack;

   -------------------
   -- Compute_Usage --
   -------------------

   function Compute_Usage (Stats : Stack_Stats) return Natural is
      Stack : Storage_Array (1 .. Storage_Offset (Stats.Stack_Size));
      for Stack'Address use Stats.Stack_Base;
      Used : Natural := 0;
   begin
      -- Count from bottom until we hit the fill pattern
      for I in reverse Stack'Range loop
         if Stack (I) /= Fill_Pattern then
            Used := Natural (I);
            exit;
         end if;
      end loop;
      return Used;
   end Compute_Usage;

end System.Stack_Usage;
