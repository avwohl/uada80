-- System.Finalization_Masters body for Z80
-- Finalization list management implementation

with Ada.Unchecked_Deallocation;

package body System.Finalization_Masters is

   procedure Free is new Ada.Unchecked_Deallocation (Fin_Node, Fin_Node_Ptr);

   ------------
   -- Attach --
   ------------

   procedure Attach
     (N : not null access Ada.Finalization.Controlled'Class;
      L : not null access Finalization_Master)
   is
   begin
      Attach_Unprotected (N, L);
   end Attach;

   ------------------------
   -- Attach_Unprotected --
   ------------------------

   procedure Attach_Unprotected
     (N : not null access Ada.Finalization.Controlled'Class;
      L : not null access Finalization_Master)
   is
      New_Node : Fin_Node_Ptr;
   begin
      if L.Finalization_Started then
         return;
      end if;

      New_Node := new Fin_Node'(Object => N, Next => null, Prev => L.Tail);

      if L.Tail /= null then
         L.Tail.Next := New_Node;
      else
         L.Head := New_Node;
      end if;

      L.Tail := New_Node;
   end Attach_Unprotected;

   ------------
   -- Detach --
   ------------

   procedure Detach
     (N : not null access Ada.Finalization.Controlled'Class)
   is
   begin
      Detach_Unprotected (N);
   end Detach;

   ------------------------
   -- Detach_Unprotected --
   ------------------------

   procedure Detach_Unprotected
     (N : not null access Ada.Finalization.Controlled'Class)
   is
      pragma Unreferenced (N);
   begin
      -- Simplified: actual implementation would search and remove
      -- For Z80 memory constraints, we use simplified tracking
      null;
   end Detach_Unprotected;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Master : in Out Finalization_Master) is
      Node : Fin_Node_Ptr := Master.Tail;
      Next : Fin_Node_Ptr;
   begin
      Master.Finalization_Started := True;

      -- Finalize in reverse order (LIFO)
      while Node /= null loop
         begin
            Ada.Finalization.Finalize (Node.Object.all);
         exception
            when others => null;  -- Ignore finalization exceptions
         end;
         Next := Node.Prev;
         Free (Node);
         Node := Next;
      end loop;

      Master.Head := null;
      Master.Tail := null;
      Master.Finalization_Started := False;
   end Finalize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Master : Finalization_Master) return Boolean is
   begin
      return Master.Head = null;
   end Is_Empty;

   --------------------------
   -- Set_Finalize_Address --
   --------------------------

   procedure Set_Finalize_Address
     (Master  : in Out Finalization_Master;
      Address : System.Address)
   is
   begin
      Master.Fin_Address := Address;
   end Set_Finalize_Address;

   ----------------------
   -- Finalize_Address --
   ----------------------

   function Finalize_Address
     (Master : Finalization_Master) return System.Address
   is
   begin
      return Master.Fin_Address;
   end Finalize_Address;

end System.Finalization_Masters;
