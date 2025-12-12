-- GNAT.Bounded_Buffers body for Z80
-- Bounded buffer implementation

with System.Tasking;

package body GNAT.Bounded_Buffers is

   ------------
   -- Insert --
   ------------

   procedure Insert (B : in Out Bounded_Buffer; Item : Element) is
   begin
      -- Wait for space
      while B.Count >= B.Size loop
         System.Tasking.Yield;
      end loop;

      System.Tasking.Enter_Protected;
      B.Buffer (B.Tail) := Item;
      B.Tail := (if B.Tail = B.Size then 1 else B.Tail + 1);
      B.Count := B.Count + 1;
      System.Tasking.Leave_Protected;
   end Insert;

   ------------
   -- Remove --
   ------------

   procedure Remove (B : in Out Bounded_Buffer; Item : out Element) is
   begin
      -- Wait for element
      while B.Count = 0 loop
         System.Tasking.Yield;
      end loop;

      System.Tasking.Enter_Protected;
      Item := B.Buffer (B.Head);
      B.Head := (if B.Head = B.Size then 1 else B.Head + 1);
      B.Count := B.Count - 1;
      System.Tasking.Leave_Protected;
   end Remove;

   ----------------
   -- Try_Insert --
   ----------------

   procedure Try_Insert
     (B       : in Out Bounded_Buffer;
      Item    : Element;
      Success : out Boolean)
   is
   begin
      System.Tasking.Enter_Protected;
      if B.Count >= B.Size then
         Success := False;
      else
         B.Buffer (B.Tail) := Item;
         B.Tail := (if B.Tail = B.Size then 1 else B.Tail + 1);
         B.Count := B.Count + 1;
         Success := True;
      end if;
      System.Tasking.Leave_Protected;
   end Try_Insert;

   ----------------
   -- Try_Remove --
   ----------------

   procedure Try_Remove
     (B       : in Out Bounded_Buffer;
      Item    : out Element;
      Success : out Boolean)
   is
   begin
      System.Tasking.Enter_Protected;
      if B.Count = 0 then
         Success := False;
      else
         Item := B.Buffer (B.Head);
         B.Head := (if B.Head = B.Size then 1 else B.Head + 1);
         B.Count := B.Count - 1;
         Success := True;
      end if;
      System.Tasking.Leave_Protected;
   end Try_Remove;

   -----------
   -- Count --
   -----------

   function Count (B : Bounded_Buffer) return Natural is
   begin
      return B.Count;
   end Count;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (B : Bounded_Buffer) return Boolean is
   begin
      return B.Count = 0;
   end Is_Empty;

   -------------
   -- Is_Full --
   -------------

   function Is_Full (B : Bounded_Buffer) return Boolean is
   begin
      return B.Count >= B.Size;
   end Is_Full;

end GNAT.Bounded_Buffers;
