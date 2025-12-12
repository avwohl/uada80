-- System.Finalization for Z80
-- Controlled types support

package System.Finalization is
   pragma Preelaborate;

   -- Finalization list node
   type Finalizable;
   type Finalizable_Ptr is access all Finalizable'Class;

   type Finalizable is abstract tagged limited record
      Next : Finalizable_Ptr := null;
      Prev : Finalizable_Ptr := null;
   end record;

   -- Finalization operations
   procedure Initialize (Object : in Out Finalizable) is abstract;
   procedure Finalize (Object : in Out Finalizable) is abstract;
   procedure Adjust (Object : in Out Finalizable) is null;

   -- Finalization list
   type Finalization_List is limited private;

   procedure Attach (L : in Out Finalization_List; Obj : Finalizable_Ptr);
   procedure Detach (L : in Out Finalization_List; Obj : Finalizable_Ptr);
   procedure Finalize_List (L : in Out Finalization_List);

private
   type Finalization_List is limited record
      Head : Finalizable_Ptr := null;
      Tail : Finalizable_Ptr := null;
   end record;

end System.Finalization;
