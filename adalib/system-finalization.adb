-- System.Finalization body for Z80
-- Controlled types implementation

package body System.Finalization is

   ------------
   -- Attach --
   ------------

   procedure Attach (L : in Out Finalization_List; Obj : Finalizable_Ptr) is
   begin
      if Obj = null then
         return;
      end if;

      Obj.Prev := L.Tail;
      Obj.Next := null;

      if L.Tail /= null then
         L.Tail.Next := Obj;
      else
         L.Head := Obj;
      end if;

      L.Tail := Obj;
   end Attach;

   ------------
   -- Detach --
   ------------

   procedure Detach (L : in Out Finalization_List; Obj : Finalizable_Ptr) is
   begin
      if Obj = null then
         return;
      end if;

      if Obj.Prev /= null then
         Obj.Prev.Next := Obj.Next;
      else
         L.Head := Obj.Next;
      end if;

      if Obj.Next /= null then
         Obj.Next.Prev := Obj.Prev;
      else
         L.Tail := Obj.Prev;
      end if;

      Obj.Prev := null;
      Obj.Next := null;
   end Detach;

   -------------------
   -- Finalize_List --
   -------------------

   procedure Finalize_List (L : in Out Finalization_List) is
      Current : Finalizable_Ptr := L.Tail;
      Prev    : Finalizable_Ptr;
   begin
      -- Finalize in reverse order of creation
      while Current /= null loop
         Prev := Current.Prev;
         Finalize (Current.all);
         Current := Prev;
      end loop;

      L.Head := null;
      L.Tail := null;
   end Finalize_List;

end System.Finalization;
