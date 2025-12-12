-- Ada.Containers.Bounded_Holders body for Z80
-- Bounded indefinite holders implementation

package body Ada.Containers.Bounded_Holders is

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Holder) return Boolean is
   begin
      return Container.Empty;
   end Is_Empty;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in Out Holder) is
   begin
      Container.Empty := True;
   end Clear;

   -------------
   -- Element --
   -------------

   function Element (Container : Holder) return Element_Type is
   begin
      if Container.Empty then
         raise Constraint_Error with "Holder is empty";
      end if;
      return Container.Data;
   end Element;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in Out Holder;
      New_Item  : Element_Type)
   is
   begin
      Container.Data := New_Item;
      Container.Empty := False;
   end Replace_Element;

   ---------------
   -- To_Holder --
   ---------------

   function To_Holder (New_Item : Element_Type) return Holder is
   begin
      return (Data => New_Item, Empty => False);
   end To_Holder;

   ----------
   -- Move --
   ----------

   procedure Move (Target, Source : in Out Holder) is
   begin
      if not Source.Empty then
         Target.Data := Source.Data;
         Target.Empty := False;
         Source.Empty := True;
      else
         Target.Empty := True;
      end if;
   end Move;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Holder) return Boolean is
   begin
      if Left.Empty and Right.Empty then
         return True;
      elsif Left.Empty or Right.Empty then
         return False;
      else
         return Left.Data = Right.Data;
      end if;
   end "=";

end Ada.Containers.Bounded_Holders;
