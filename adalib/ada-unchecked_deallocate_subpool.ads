-- Ada.Unchecked_Deallocate_Subpool for Z80
-- Subpool deallocation

with System.Storage_Pools.Subpools;

generic
procedure Ada.Unchecked_Deallocate_Subpool
  (Subpool : in out System.Storage_Pools.Subpools.Subpool_Handle);
pragma Preelaborate (Ada.Unchecked_Deallocate_Subpool);
--  Deallocate a storage subpool
