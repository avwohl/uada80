-- Ada.Unchecked_Deallocation for Z80
-- Provides explicit memory deallocation for access types

with System;

generic
   type Object (<>) is limited private;
   type Name is access Object;
procedure Ada.Unchecked_Deallocation (X : in out Name);
pragma Preelaborate (Ada.Unchecked_Deallocation);
pragma Convention (Intrinsic, Ada.Unchecked_Deallocation);
