-- System.Finalization_Masters for Z80
-- Finalization list management for controlled types

with Ada.Finalization;
with System.Storage_Elements;

package System.Finalization_Masters is
   pragma Preelaborate;

   -- Finalization master for controlling finalization of allocated objects
   type Finalization_Master is tagged limited private;
   type Finalization_Master_Ptr is access all Finalization_Master'Class;

   -- Attach an object to a finalization master
   procedure Attach
     (N : not null access Ada.Finalization.Controlled'Class;
      L : not null access Finalization_Master);

   procedure Attach_Unprotected
     (N : not null access Ada.Finalization.Controlled'Class;
      L : not null access Finalization_Master);

   -- Detach an object from its finalization master
   procedure Detach
     (N : not null access Ada.Finalization.Controlled'Class);

   procedure Detach_Unprotected
     (N : not null access Ada.Finalization.Controlled'Class);

   -- Finalize all objects in the master
   procedure Finalize (Master : in out Finalization_Master);

   -- Check if master is empty
   function Is_Empty (Master : Finalization_Master) return Boolean;

   -- Set/Get finalization started flag
   procedure Set_Finalize_Address
     (Master  : in Out Finalization_Master;
      Address : System.Address);

   function Finalize_Address
     (Master : Finalization_Master) return System.Address;

   -- Base finalization master for standard pools
   Base_Pool_Master : aliased Finalization_Master;

private

   -- Simple linked list node for finalization chain
   type Fin_Node;
   type Fin_Node_Ptr is access all Fin_Node;

   type Fin_Node is record
      Object : access Ada.Finalization.Controlled'Class;
      Next   : Fin_Node_Ptr;
      Prev   : Fin_Node_Ptr;
   end record;

   type Finalization_Master is tagged limited record
      Head         : Fin_Node_Ptr := null;
      Tail         : Fin_Node_Ptr := null;
      Fin_Address  : System.Address := System.Null_Address;
      Finalization_Started : Boolean := False;
   end record;

end System.Finalization_Masters;
