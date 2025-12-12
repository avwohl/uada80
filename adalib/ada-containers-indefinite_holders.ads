-- Ada.Containers.Indefinite_Holders for Z80
-- Generic holder for indefinite types
--
-- Provides a way to store indefinite types (like String) in definite containers

with Ada.Finalization;

generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Indefinite_Holders is
   pragma Preelaborate;

   type Holder is tagged private;
   pragma Preelaborable_Initialization (Holder);

   Empty_Holder : constant Holder;

   -- Query operations
   function Is_Empty (Container : Holder) return Boolean;
   procedure Clear (Container : in Out Holder);

   -- Element access
   function Element (Container : Holder) return Element_Type;

   procedure Replace_Element
     (Container : in Out Holder;
      New_Item  : Element_Type);

   procedure Query_Element
     (Container : Holder;
      Process   : not null access procedure (Element : Element_Type));

   procedure Update_Element
     (Container : in Out Holder;
      Process   : not null access procedure (Element : in Out Element_Type));

   -- Assignment and construction
   function To_Holder (New_Item : Element_Type) return Holder;

   -- Comparison
   function "=" (Left, Right : Holder) return Boolean;

   -- Move semantics
   procedure Move (Target : in Out Holder; Source : in Out Holder);

private

   type Element_Access is access Element_Type;

   type Holder is new Ada.Finalization.Controlled with record
      Element : Element_Access := null;
   end record;

   overriding procedure Initialize (Container : in Out Holder);
   overriding procedure Adjust (Container : in Out Holder);
   overriding procedure Finalize (Container : in Out Holder);

   Empty_Holder : constant Holder :=
     (Ada.Finalization.Controlled with Element => null);

end Ada.Containers.Indefinite_Holders;
