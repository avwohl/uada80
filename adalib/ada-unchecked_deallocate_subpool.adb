-- Ada.Unchecked_Deallocate_Subpool body for Z80
-- Subpool deallocation implementation

procedure Ada.Unchecked_Deallocate_Subpool
  (Subpool : in out System.Storage_Pools.Subpools.Subpool_Handle)
is
   use System.Storage_Pools.Subpools;
begin
   if Subpool /= null then
      Deallocate_Subpool (Pool_Of_Subpool (Subpool).all, Subpool);
      Subpool := null;
   end if;
end Ada.Unchecked_Deallocate_Subpool;
