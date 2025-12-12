-- Ada.Containers.Bounded_Ordered_Sets body for Z80
-- Generic bounded ordered set implementation using sorted array

package body Ada.Containers.Bounded_Ordered_Sets is

   function Equivalent (Left, Right : Element_Type) return Boolean is
   begin
      return not (Left < Right) and not (Right < Left);
   end Equivalent;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Set) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      for I in 1 .. Left.Length loop
         if Left.Elements (I) /= Right.Elements (I) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   ------------
   -- Length --
   ------------

   function Length (Container : Set) return Count_Type is
   begin
      return Container.Length;
   end Length;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Set) return Boolean is
   begin
      return Container.Length = 0;
   end Is_Empty;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in Out Set) is
   begin
      Container.Length := 0;
   end Clear;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position.Index = 0 then
         raise Constraint_Error;
      end if;
      return Position.Container.Elements (Position.Index);
   end Element;

   -----------
   -- First --
   -----------

   function First (Container : Set) return Cursor is
   begin
      if Container.Length = 0 then
         return No_Element;
      end if;
      return (Container => Container'Unchecked_Access, Index => 1);
   end First;

   ----------
   -- Last --
   ----------

   function Last (Container : Set) return Cursor is
   begin
      if Container.Length = 0 then
         return No_Element;
      end if;
      return (Container => Container'Unchecked_Access, Index => Container.Length);
   end Last;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Index = 0 or Position.Index >= Position.Container.Length then
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
      if Position.Index <= 1 then
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
      return Position.Index /= 0;
   end Has_Element;

   ----------
   -- Find --
   ----------

   function Find (Container : Set; Item : Element_Type) return Cursor is
   begin
      if Container.Length = 0 then
         return No_Element;
      end if;

      declare
         Low  : Count_Type := 1;
         High : Count_Type := Container.Length;
         Mid  : Count_Type;
      begin
         while Low <= High loop
            Mid := (Low + High) / 2;
            if Equivalent (Container.Elements (Mid), Item) then
               return (Container => Container'Unchecked_Access, Index => Mid);
            elsif Container.Elements (Mid) < Item then
               Low := Mid + 1;
            else
               if Mid = 1 then
                  exit;
               end if;
               High := Mid - 1;
            end if;
         end loop;
      end;

      return No_Element;
   end Find;

   --------------
   -- Contains --
   --------------

   function Contains (Container : Set; Item : Element_Type) return Boolean is
   begin
      return Find (Container, Item) /= No_Element;
   end Contains;

   -----------
   -- Floor --
   -----------

   function Floor (Container : Set; Item : Element_Type) return Cursor is
      Result : Cursor := No_Element;
   begin
      for I in 1 .. Container.Length loop
         if Container.Elements (I) < Item or
            Equivalent (Container.Elements (I), Item)
         then
            Result := (Container => Container'Unchecked_Access, Index => I);
         else
            exit;
         end if;
      end loop;
      return Result;
   end Floor;

   -------------
   -- Ceiling --
   -------------

   function Ceiling (Container : Set; Item : Element_Type) return Cursor is
   begin
      for I in 1 .. Container.Length loop
         if not (Container.Elements (I) < Item) then
            return (Container => Container'Unchecked_Access, Index => I);
         end if;
      end loop;
      return No_Element;
   end Ceiling;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in Out Set;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean)
   is
      Insert_Pos : Count_Type := Container.Length + 1;
   begin
      for I in 1 .. Container.Length loop
         if Equivalent (Container.Elements (I), New_Item) then
            Position := (Container => Container'Unchecked_Access, Index => I);
            Inserted := False;
            return;
         elsif New_Item < Container.Elements (I) then
            Insert_Pos := I;
            exit;
         end if;
      end loop;

      if Container.Length >= Container.Capacity then
         raise Capacity_Error;
      end if;

      for I in reverse Insert_Pos .. Container.Length loop
         Container.Elements (I + 1) := Container.Elements (I);
      end loop;

      Container.Elements (Insert_Pos) := New_Item;
      Container.Length := Container.Length + 1;

      Position := (Container => Container'Unchecked_Access, Index => Insert_Pos);
      Inserted := True;
   end Insert;

   procedure Insert (Container : in Out Set; New_Item : Element_Type) is
      Position : Cursor;
      Inserted : Boolean;
   begin
      Insert (Container, New_Item, Position, Inserted);
      if not Inserted then
         raise Constraint_Error;
      end if;
   end Insert;

   -------------
   -- Include --
   -------------

   procedure Include (Container : in Out Set; New_Item : Element_Type) is
      Position : Cursor;
      Inserted : Boolean;
   begin
      Insert (Container, New_Item, Position, Inserted);
   end Include;

   -------------
   -- Replace --
   -------------

   procedure Replace (Container : in Out Set; New_Item : Element_Type) is
      Position : constant Cursor := Find (Container, New_Item);
   begin
      if Position = No_Element then
         raise Constraint_Error;
      end if;
      Container.Elements (Position.Index) := New_Item;
   end Replace;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Container : in Out Set; Item : Element_Type) is
      Position : constant Cursor := Find (Container, Item);
   begin
      if Position /= No_Element then
         for I in Position.Index .. Container.Length - 1 loop
            Container.Elements (I) := Container.Elements (I + 1);
         end loop;
         Container.Length := Container.Length - 1;
      end if;
   end Exclude;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in Out Set; Item : Element_Type) is
   begin
      if not Contains (Container, Item) then
         raise Constraint_Error;
      end if;
      Exclude (Container, Item);
   end Delete;

   procedure Delete (Container : in Out Set; Position : in Out Cursor) is
   begin
      if Position.Index = 0 then
         raise Constraint_Error;
      end if;
      for I in Position.Index .. Container.Length - 1 loop
         Container.Elements (I) := Container.Elements (I + 1);
      end loop;
      Container.Length := Container.Length - 1;
      Position := No_Element;
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (Container : in Out Set) is
   begin
      if Container.Length > 0 then
         for I in 1 .. Container.Length - 1 loop
            Container.Elements (I) := Container.Elements (I + 1);
         end loop;
         Container.Length := Container.Length - 1;
      end if;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Container : in Out Set) is
   begin
      if Container.Length > 0 then
         Container.Length := Container.Length - 1;
      end if;
   end Delete_Last;

   -----------
   -- Union --
   -----------

   procedure Union (Target : in Out Set; Source : Set) is
   begin
      for I in 1 .. Source.Length loop
         Include (Target, Source.Elements (I));
      end loop;
   end Union;

   ------------------
   -- Intersection --
   ------------------

   procedure Intersection (Target : in Out Set; Source : Set) is
      I : Count_Type := 1;
   begin
      while I <= Target.Length loop
         if not Contains (Source, Target.Elements (I)) then
            for J in I .. Target.Length - 1 loop
               Target.Elements (J) := Target.Elements (J + 1);
            end loop;
            Target.Length := Target.Length - 1;
         else
            I := I + 1;
         end if;
      end loop;
   end Intersection;

   ----------------
   -- Difference --
   ----------------

   procedure Difference (Target : in Out Set; Source : Set) is
   begin
      for I in 1 .. Source.Length loop
         Exclude (Target, Source.Elements (I));
      end loop;
   end Difference;

   --------------------------
   -- Symmetric_Difference --
   --------------------------

   procedure Symmetric_Difference (Target : in Out Set; Source : Set) is
   begin
      for I in 1 .. Source.Length loop
         if Contains (Target, Source.Elements (I)) then
            Exclude (Target, Source.Elements (I));
         else
            Include (Target, Source.Elements (I));
         end if;
      end loop;
   end Symmetric_Difference;

   ---------------
   -- Is_Subset --
   ---------------

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean is
   begin
      for I in 1 .. Subset.Length loop
         if not Contains (Of_Set, Subset.Elements (I)) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Subset;

   -------------
   -- Overlap --
   -------------

   function Overlap (Left, Right : Set) return Boolean is
   begin
      for I in 1 .. Left.Length loop
         if Contains (Right, Left.Elements (I)) then
            return True;
         end if;
      end loop;
      return False;
   end Overlap;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Set;
      Process   : not null access procedure (Position : Cursor))
   is
   begin
      for I in 1 .. Container.Length loop
         Process ((Container => Container'Unchecked_Access, Index => I));
      end loop;
   end Iterate;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (Container : Set;
      Process   : not null access procedure (Position : Cursor))
   is
   begin
      for I in reverse 1 .. Container.Length loop
         Process ((Container => Container'Unchecked_Access, Index => I));
      end loop;
   end Reverse_Iterate;

end Ada.Containers.Bounded_Ordered_Sets;
