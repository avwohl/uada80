-- Ada.Containers.Indefinite_Holders body for Z80
-- Generic holder implementation

with Ada.Unchecked_Deallocation;

package body Ada.Containers.Indefinite_Holders is

   procedure Free is new Ada.Unchecked_Deallocation
     (Element_Type, Element_Access);

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Container : in Out Holder) is
   begin
      Container.Element := null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Container : in Out Holder) is
   begin
      if Container.Element /= null then
         Container.Element := new Element_Type'(Container.Element.all);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Container : in Out Holder) is
   begin
      if Container.Element /= null then
         Free (Container.Element);
      end if;
   end Finalize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Holder) return Boolean is
   begin
      return Container.Element = null;
   end Is_Empty;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in Out Holder) is
   begin
      if Container.Element /= null then
         Free (Container.Element);
      end if;
   end Clear;

   -------------
   -- Element --
   -------------

   function Element (Container : Holder) return Element_Type is
   begin
      if Container.Element = null then
         raise Constraint_Error with "Holder is empty";
      end if;
      return Container.Element.all;
   end Element;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in Out Holder;
      New_Item  : Element_Type)
   is
   begin
      if Container.Element /= null then
         Free (Container.Element);
      end if;
      Container.Element := new Element_Type'(New_Item);
   end Replace_Element;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Container : Holder;
      Process   : not null access procedure (Element : Element_Type))
   is
   begin
      if Container.Element = null then
         raise Constraint_Error with "Holder is empty";
      end if;
      Process (Container.Element.all);
   end Query_Element;

   --------------------
   -- Update_Element --
   --------------------

   procedure Update_Element
     (Container : in Out Holder;
      Process   : not null access procedure (Element : in Out Element_Type))
   is
   begin
      if Container.Element = null then
         raise Constraint_Error with "Holder is empty";
      end if;
      Process (Container.Element.all);
   end Update_Element;

   ---------------
   -- To_Holder --
   ---------------

   function To_Holder (New_Item : Element_Type) return Holder is
      Result : Holder;
   begin
      Result.Element := new Element_Type'(New_Item);
      return Result;
   end To_Holder;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Holder) return Boolean is
   begin
      if Left.Element = null and Right.Element = null then
         return True;
      elsif Left.Element = null or Right.Element = null then
         return False;
      else
         return Left.Element.all = Right.Element.all;
      end if;
   end "=";

   ----------
   -- Move --
   ----------

   procedure Move (Target : in Out Holder; Source : in Out Holder) is
   begin
      if Target.Element /= null then
         Free (Target.Element);
      end if;
      Target.Element := Source.Element;
      Source.Element := null;
   end Move;

end Ada.Containers.Indefinite_Holders;
