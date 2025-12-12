-- Ada.Containers.Indefinite_Vectors body for Z80
-- Generic vector implementation for indefinite types

with Ada.Unchecked_Deallocation;

package body Ada.Containers.Indefinite_Vectors is

   procedure Free is new Ada.Unchecked_Deallocation
     (Element_Type, Element_Access);

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Vector) return Boolean is
   begin
      if Left.Last /= Right.Last then
         return False;
      end if;

      for I in Index_Type'First .. Left.Last loop
         if Left.Elements (I) = null or Right.Elements (I) = null then
            if Left.Elements (I) /= Right.Elements (I) then
               return False;
            end if;
         elsif Left.Elements (I).all /= Right.Elements (I).all then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   ------------
   -- Length --
   ------------

   function Length (Container : Vector) return Count_Type is
   begin
      if Container.Last = No_Index then
         return 0;
      else
         return Count_Type (Container.Last - Index_Type'First + 1);
      end if;
   end Length;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Vector) return Boolean is
   begin
      return Container.Last = No_Index;
   end Is_Empty;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in Out Vector) is
   begin
      -- Free all allocated elements
      for I in Index_Type'First .. Container.Last loop
         if Container.Elements (I) /= null then
            Free (Container.Elements (I));
         end if;
      end loop;
      Container.Last := No_Index;
   end Clear;

   -------------
   -- Element --
   -------------

   function Element (Container : Vector; Index : Index_Type) return Element_Type is
   begin
      if Index > Container.Last then
         raise Constraint_Error;
      end if;
      if Container.Elements (Index) = null then
         raise Constraint_Error;
      end if;
      return Container.Elements (Index).all;
   end Element;

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position.Container = null or Position.Index = No_Index then
         raise Constraint_Error;
      end if;
      return Position.Container.Elements (Position.Index).all;
   end Element;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in Out Vector;
      Index     : Index_Type;
      New_Item  : Element_Type)
   is
   begin
      if Index > Container.Last then
         raise Constraint_Error;
      end if;
      if Container.Elements (Index) /= null then
         Free (Container.Elements (Index));
      end if;
      Container.Elements (Index) := new Element_Type'(New_Item);
   end Replace_Element;

   procedure Replace_Element
     (Container : in Out Vector;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      Replace_Element (Container, Position.Index, New_Item);
   end Replace_Element;

   -----------------
   -- First_Index --
   -----------------

   function First_Index (Container : Vector) return Index_Type is
      pragma Unreferenced (Container);
   begin
      return Index_Type'First;
   end First_Index;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (Container : Vector) return Extended_Index is
   begin
      return Container.Last;
   end Last_Index;

   -----------
   -- First --
   -----------

   function First (Container : Vector) return Cursor is
   begin
      if Container.Last = No_Index then
         return No_Element;
      end if;
      return (Container => Container'Unchecked_Access, Index => Index_Type'First);
   end First;

   ----------
   -- Last --
   ----------

   function Last (Container : Vector) return Cursor is
   begin
      if Container.Last = No_Index then
         return No_Element;
      end if;
      return (Container => Container'Unchecked_Access, Index => Container.Last);
   end Last;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Index = No_Index or Position.Container = null then
         return No_Element;
      end if;
      if Position.Index >= Position.Container.Last then
         return No_Element;
      end if;
      return (Container => Position.Container, Index => Position.Index + 1);
   end Next;

   procedure Next (Position : in Out Cursor) is
   begin
      Position := Next (Position);
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous (Position : Cursor) return Cursor is
   begin
      if Position.Index = No_Index or Position.Container = null then
         return No_Element;
      end if;
      if Position.Index <= Index_Type'First then
         return No_Element;
      end if;
      return (Container => Position.Container, Index => Position.Index - 1);
   end Previous;

   procedure Previous (Position : in Out Cursor) is
   begin
      Position := Previous (Position);
   end Previous;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Index /= No_Index and Position.Container /= null;
   end Has_Element;

   ----------------
   -- Find_Index --
   ----------------

   function Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'First) return Extended_Index
   is
   begin
      for I in Index .. Container.Last loop
         if Container.Elements (I) /= null and then
            Container.Elements (I).all = Item
         then
            return I;
         end if;
      end loop;
      return No_Index;
   end Find_Index;

   ----------
   -- Find --
   ----------

   function Find
     (Container : Vector;
      Item      : Element_Type) return Cursor
   is
      Idx : Extended_Index := Find_Index (Container, Item);
   begin
      if Idx = No_Index then
         return No_Element;
      end if;
      return (Container => Container'Unchecked_Access, Index => Idx);
   end Find;

   --------------
   -- Contains --
   --------------

   function Contains (Container : Vector; Item : Element_Type) return Boolean is
   begin
      return Find_Index (Container, Item) /= No_Index;
   end Contains;

   ------------
   -- Append --
   ------------

   procedure Append (Container : in Out Vector; New_Item : Element_Type) is
      New_Last : Extended_Index;
   begin
      if Container.Last = No_Index then
         New_Last := Index_Type'First;
      else
         New_Last := Container.Last + 1;
      end if;

      if New_Last > Index_Type'First + Index_Type'Base (Max_Capacity) - 1 then
         raise Capacity_Error;
      end if;

      Container.Elements (New_Last) := new Element_Type'(New_Item);
      Container.Last := New_Last;
   end Append;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (Container : in Out Vector; New_Item : Element_Type) is
   begin
      Insert (Container, Index_Type'First, New_Item);
   end Prepend;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in Out Vector;
      Before    : Extended_Index;
      New_Item  : Element_Type)
   is
      New_Last : Extended_Index;
   begin
      if Container.Last = No_Index then
         New_Last := Index_Type'First;
      else
         New_Last := Container.Last + 1;
      end if;

      if New_Last > Index_Type'First + Index_Type'Base (Max_Capacity) - 1 then
         raise Capacity_Error;
      end if;

      -- Shift elements to make room
      for I in reverse Before .. Container.Last loop
         Container.Elements (I + 1) := Container.Elements (I);
      end loop;

      Container.Elements (Before) := new Element_Type'(New_Item);
      Container.Last := New_Last;
   end Insert;

   procedure Insert
     (Container : in Out Vector;
      Before    : Cursor;
      New_Item  : Element_Type)
   is
   begin
      if Before = No_Element then
         Append (Container, New_Item);
      else
         Insert (Container, Before.Index, New_Item);
      end if;
   end Insert;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in Out Vector; Index : Extended_Index) is
   begin
      if Index > Container.Last then
         raise Constraint_Error;
      end if;

      -- Free the element
      if Container.Elements (Index) /= null then
         Free (Container.Elements (Index));
      end if;

      -- Shift elements down
      for I in Index .. Container.Last - 1 loop
         Container.Elements (I) := Container.Elements (I + 1);
      end loop;

      Container.Elements (Container.Last) := null;

      if Container.Last = Index_Type'First then
         Container.Last := No_Index;
      else
         Container.Last := Container.Last - 1;
      end if;
   end Delete;

   procedure Delete (Container : in Out Vector; Position : in Out Cursor) is
   begin
      Delete (Container, Position.Index);
      Position := No_Element;
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (Container : in Out Vector) is
   begin
      if Container.Last /= No_Index then
         Delete (Container, Index_Type'First);
      end if;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Container : in Out Vector) is
   begin
      if Container.Last /= No_Index then
         if Container.Elements (Container.Last) /= null then
            Free (Container.Elements (Container.Last));
         end if;
         if Container.Last = Index_Type'First then
            Container.Last := No_Index;
         else
            Container.Last := Container.Last - 1;
         end if;
      end if;
   end Delete_Last;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Vector;
      Process   : not null access procedure (Position : Cursor))
   is
   begin
      if Container.Last = No_Index then
         return;
      end if;

      for I in Index_Type'First .. Container.Last loop
         Process ((Container => Container'Unchecked_Access, Index => I));
      end loop;
   end Iterate;

end Ada.Containers.Indefinite_Vectors;
