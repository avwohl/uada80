-- Ada.Containers.Bounded_Holders for Z80
-- Bounded indefinite holders (Ada 2012)

generic
   type Element_Type (<>) is private;
   Max_Element_Size : Natural;
package Ada.Containers.Bounded_Holders is
   pragma Preelaborate;

   type Holder is private;

   Empty_Holder : constant Holder;

   function Is_Empty (Container : Holder) return Boolean;

   procedure Clear (Container : in out Holder);

   function Element (Container : Holder) return Element_Type;

   procedure Replace_Element
     (Container : in Out Holder;
      New_Item  : Element_Type);

   function To_Holder (New_Item : Element_Type) return Holder;

   procedure Move (Target, Source : in Out Holder);

   function "=" (Left, Right : Holder) return Boolean;

private
   type Element_Access is access all Element_Type;

   type Holder is record
      Data  : aliased Element_Type;
      Empty : Boolean := True;
   end record;

   Empty_Holder : constant Holder := (Data => <>, Empty => True);

end Ada.Containers.Bounded_Holders;
