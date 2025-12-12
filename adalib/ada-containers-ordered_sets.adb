-- Ada.Containers.Ordered_Sets body for Z80
-- Generic ordered set implementation

package body Ada.Containers.Ordered_Sets is

   -- Internal: Binary search for element position
   function Find_Position (Container : Set; Item : Element_Type) return Element_Index is
      Low   : Element_Index := 1;
      High  : Element_Index := Element_Index (Container.Length);
      Mid   : Element_Index;
   begin
      if Container.Length = 0 then
         return 1;
      end if;

      while Low <= High loop
         Mid := (Low + High) / 2;

         if Container.Elements (Mid) = Item then
            return Mid;
         elsif Container.Elements (Mid) < Item then
            Low := Mid + 1;
         else
            High := Mid - 1;
         end if;
      end loop;

      return Low;  -- Insert position
   end Find_Position;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Set) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      for I in 1 .. Element_Index (Left.Length) loop
         if not (Left.Elements (I) = Right.Elements (I)) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   ---------------------
   -- Equivalent_Sets --
   ---------------------

   function Equivalent_Sets (Left, Right : Set) return Boolean is
   begin
      return Left = Right;
   end Equivalent_Sets;

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
      if Position.Index = Null_Index or Position.Container = null then
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
      return (Container => Container'Unchecked_Access, Index => Element_Index (Container.Length));
   end Last;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Index = Null_Index or Position.Container = null then
         return No_Element;
      end if;

      if Position.Index >= Element_Index (Position.Container.Length) then
         return No_Element;
      end if;

      return (Container => Position.Container, Index => Position.Index + 1);
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous (Position : Cursor) return Cursor is
   begin
      if Position.Index = Null_Index or Position.Container = null then
         return No_Element;
      end if;

      if Position.Index <= 1 then
         return No_Element;
      end if;

      return (Container => Position.Container, Index => Position.Index - 1);
   end Previous;

   procedure Next (Position : in Out Cursor) is
   begin
      Position := Next (Position);
   end Next;

   procedure Previous (Position : in Out Cursor) is
   begin
      Position := Previous (Position);
   end Previous;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Index /= Null_Index and Position.Container /= null;
   end Has_Element;

   ----------
   -- Find --
   ----------

   function Find (Container : Set; Item : Element_Type) return Cursor is
      Pos : Element_Index;
   begin
      if Container.Length = 0 then
         return No_Element;
      end if;

      Pos := Find_Position (Container, Item);

      if Pos <= Element_Index (Container.Length) and then
         Container.Elements (Pos) = Item
      then
         return (Container => Container'Unchecked_Access, Index => Pos);
      end if;

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
      Pos : Element_Index;
   begin
      if Container.Length = 0 then
         return No_Element;
      end if;

      Pos := Find_Position (Container, Item);

      if Pos <= Element_Index (Container.Length) and then
         Container.Elements (Pos) = Item
      then
         return (Container => Container'Unchecked_Access, Index => Pos);
      end if;

      if Pos > 1 then
         return (Container => Container'Unchecked_Access, Index => Pos - 1);
      end if;

      return No_Element;
   end Floor;

   -------------
   -- Ceiling --
   -------------

   function Ceiling (Container : Set; Item : Element_Type) return Cursor is
      Pos : Element_Index;
   begin
      if Container.Length = 0 then
         return No_Element;
      end if;

      Pos := Find_Position (Container, Item);

      if Pos <= Element_Index (Container.Length) then
         return (Container => Container'Unchecked_Access, Index => Pos);
      end if;

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
      Pos : Element_Index;
   begin
      if Container.Length >= Count_Type (Max_Elements) then
         raise Capacity_Error;
      end if;

      Pos := Find_Position (Container, New_Item);

      -- Check for duplicate
      if Pos <= Element_Index (Container.Length) and then
         Container.Elements (Pos) = New_Item
      then
         Position := (Container => Container'Unchecked_Access, Index => Pos);
         Inserted := False;
         return;
      end if;

      -- Shift elements to make room
      for I in reverse Pos .. Element_Index (Container.Length) loop
         Container.Elements (I + 1) := Container.Elements (I);
      end loop;

      -- Insert new element
      Container.Elements (Pos) := New_Item;
      Container.Length := Container.Length + 1;

      Position := (Container => Container'Unchecked_Access, Index => Pos);
      Inserted := True;
   end Insert;

   procedure Insert
     (Container : in Out Set;
      New_Item  : Element_Type)
   is
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
      -- If not inserted, element already exists - that's fine for Include
   end Include;

   -------------
   -- Replace --
   -------------

   procedure Replace (Container : in Out Set; New_Item : Element_Type) is
      Position : Cursor := Find (Container, New_Item);
   begin
      if Position = No_Element then
         raise Constraint_Error;
      end if;
      -- For sets with only ordering (no separate key), Replace is a no-op
      -- The element is equivalent to what's already there
   end Replace;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in Out Set; Item : Element_Type) is
      Position : Cursor := Find (Container, Item);
   begin
      if Position = No_Element then
         raise Constraint_Error;
      end if;
      Delete (Container, Position);
   end Delete;

   procedure Delete (Container : in Out Set; Position : in Out Cursor) is
      Idx : Element_Index := Position.Index;
   begin
      if Position.Index = Null_Index then
         raise Constraint_Error;
      end if;

      -- Shift remaining elements down
      for I in Idx .. Element_Index (Container.Length) - 1 loop
         Container.Elements (I) := Container.Elements (I + 1);
      end loop;

      Container.Length := Container.Length - 1;
      Position := No_Element;
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (Container : in Out Set) is
      Position : Cursor := First (Container);
   begin
      if Position /= No_Element then
         Delete (Container, Position);
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

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Container : in Out Set; Item : Element_Type) is
      Position : Cursor := Find (Container, Item);
   begin
      if Position /= No_Element then
         Delete (Container, Position);
      end if;
   end Exclude;

   -----------
   -- Union --
   -----------

   procedure Union (Target : in Out Set; Source : Set) is
   begin
      for I in 1 .. Element_Index (Source.Length) loop
         Include (Target, Source.Elements (I));
      end loop;
   end Union;

   function Union (Left, Right : Set) return Set is
      Result : Set := Left;
   begin
      Union (Result, Right);
      return Result;
   end Union;

   ------------------
   -- Intersection --
   ------------------

   procedure Intersection (Target : in Out Set; Source : Set) is
      Result : Set;
   begin
      for I in 1 .. Element_Index (Target.Length) loop
         if Contains (Source, Target.Elements (I)) then
            Include (Result, Target.Elements (I));
         end if;
      end loop;
      Target := Result;
   end Intersection;

   function Intersection (Left, Right : Set) return Set is
      Result : Set;
   begin
      for I in 1 .. Element_Index (Left.Length) loop
         if Contains (Right, Left.Elements (I)) then
            Include (Result, Left.Elements (I));
         end if;
      end loop;
      return Result;
   end Intersection;

   ----------------
   -- Difference --
   ----------------

   procedure Difference (Target : in Out Set; Source : Set) is
      Result : Set;
   begin
      for I in 1 .. Element_Index (Target.Length) loop
         if not Contains (Source, Target.Elements (I)) then
            Include (Result, Target.Elements (I));
         end if;
      end loop;
      Target := Result;
   end Difference;

   function Difference (Left, Right : Set) return Set is
      Result : Set;
   begin
      for I in 1 .. Element_Index (Left.Length) loop
         if not Contains (Right, Left.Elements (I)) then
            Include (Result, Left.Elements (I));
         end if;
      end loop;
      return Result;
   end Difference;

   --------------------------
   -- Symmetric_Difference --
   --------------------------

   procedure Symmetric_Difference (Target : in Out Set; Source : Set) is
      Result : Set;
   begin
      -- Elements in Target but not in Source
      for I in 1 .. Element_Index (Target.Length) loop
         if not Contains (Source, Target.Elements (I)) then
            Include (Result, Target.Elements (I));
         end if;
      end loop;

      -- Elements in Source but not in Target
      for I in 1 .. Element_Index (Source.Length) loop
         if not Contains (Target, Source.Elements (I)) then
            Include (Result, Source.Elements (I));
         end if;
      end loop;

      Target := Result;
   end Symmetric_Difference;

   function Symmetric_Difference (Left, Right : Set) return Set is
      Result : Set := Left;
   begin
      Symmetric_Difference (Result, Right);
      return Result;
   end Symmetric_Difference;

   ---------------
   -- Is_Subset --
   ---------------

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean is
   begin
      for I in 1 .. Element_Index (Subset.Length) loop
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
      for I in 1 .. Element_Index (Left.Length) loop
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
      for I in 1 .. Element_Index (Container.Length) loop
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
      for I in reverse 1 .. Element_Index (Container.Length) loop
         Process ((Container => Container'Unchecked_Access, Index => I));
      end loop;
   end Reverse_Iterate;

end Ada.Containers.Ordered_Sets;
