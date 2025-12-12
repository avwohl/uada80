-- Ada.Containers.Hashed_Sets body for Z80
-- Generic hash-based set implementation

package body Ada.Containers.Hashed_Sets is

   -- Internal: Calculate bucket index from hash
   function Get_Bucket (Item : Element_Type) return Bucket_Index is
   begin
      return Bucket_Index (Hash (Item) mod Hash_Type (Table_Size));
   end Get_Bucket;

   -- Internal: Find bucket for element using linear probing
   function Find_Bucket
     (Container  : Set;
      Item       : Element_Type;
      For_Insert : Boolean := False) return Bucket_Index
   is
      Start_Idx     : Bucket_Index := Get_Bucket (Item);
      Idx           : Bucket_Index := Start_Idx;
      First_Deleted : Bucket_Index := Bucket_Index'Last;
      Found_Deleted : Boolean := False;
   begin
      loop
         case Container.Entries (Idx).State is
            when Empty =>
               if For_Insert and Found_Deleted then
                  return First_Deleted;
               end if;
               return Idx;

            when Deleted =>
               if For_Insert and not Found_Deleted then
                  First_Deleted := Idx;
                  Found_Deleted := True;
               end if;

            when Occupied =>
               if Equivalent_Elements (Container.Entries (Idx).Element, Item) then
                  return Idx;
               end if;
         end case;

         Idx := (Idx + 1) mod Table_Size;
         exit when Idx = Start_Idx;
      end loop;

      if For_Insert and Found_Deleted then
         return First_Deleted;
      end if;

      return Start_Idx;
   end Find_Bucket;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Set) return Boolean is
   begin
      return Equivalent_Sets (Left, Right);
   end "=";

   ---------------------
   -- Equivalent_Sets --
   ---------------------

   function Equivalent_Sets (Left, Right : Set) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      for I in Bucket_Index loop
         if Left.Entries (I).State = Occupied then
            if not Contains (Right, Left.Entries (I).Element) then
               return False;
            end if;
         end if;
      end loop;

      return True;
   end Equivalent_Sets;

   --------------
   -- Capacity --
   --------------

   function Capacity (Container : Set) return Count_Type is
      pragma Unreferenced (Container);
   begin
      return Count_Type (Table_Size);
   end Capacity;

   ----------------------
   -- Reserve_Capacity --
   ----------------------

   procedure Reserve_Capacity (Container : in Out Set; Capacity : Count_Type) is
      pragma Unreferenced (Container);
      pragma Unreferenced (Capacity);
   begin
      null;  -- Fixed size on Z80
   end Reserve_Capacity;

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
      for I in Bucket_Index loop
         Container.Entries (I).State := Empty;
      end loop;
      Container.Length := 0;
   end Clear;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position.Container = null or else
         Position.Container.Entries (Position.Index).State /= Occupied
      then
         raise Constraint_Error;
      end if;
      return Position.Container.Entries (Position.Index).Element;
   end Element;

   -----------
   -- First --
   -----------

   function First (Container : Set) return Cursor is
   begin
      for I in Bucket_Index loop
         if Container.Entries (I).State = Occupied then
            return (Container => Container'Unchecked_Access, Index => I);
         end if;
      end loop;
      return No_Element;
   end First;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Container = null then
         return No_Element;
      end if;

      for I in Position.Index + 1 .. Bucket_Index'Last loop
         if Position.Container.Entries (I).State = Occupied then
            return (Container => Position.Container, Index => I);
         end if;
      end loop;

      return No_Element;
   end Next;

   procedure Next (Position : in Out Cursor) is
   begin
      Position := Next (Position);
   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Container /= null and then
             Position.Container.Entries (Position.Index).State = Occupied;
   end Has_Element;

   ----------
   -- Find --
   ----------

   function Find (Container : Set; Item : Element_Type) return Cursor is
      Idx : Bucket_Index := Find_Bucket (Container, Item);
   begin
      if Container.Entries (Idx).State = Occupied and then
         Equivalent_Elements (Container.Entries (Idx).Element, Item)
      then
         return (Container => Container'Unchecked_Access, Index => Idx);
      end if;
      return No_Element;
   end Find;

   --------------
   -- Contains --
   --------------

   function Contains (Container : Set; Item : Element_Type) return Boolean is
   begin
      return Has_Element (Find (Container, Item));
   end Contains;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in Out Set;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean)
   is
      Idx : Bucket_Index;
   begin
      if Container.Length >= Count_Type (Table_Size) - 1 then
         raise Capacity_Error;
      end if;

      Idx := Find_Bucket (Container, New_Item, For_Insert => True);

      if Container.Entries (Idx).State = Occupied and then
         Equivalent_Elements (Container.Entries (Idx).Element, New_Item)
      then
         Position := (Container => Container'Unchecked_Access, Index => Idx);
         Inserted := False;
         return;
      end if;

      Container.Entries (Idx) := (Element => New_Item, State => Occupied);
      Container.Length := Container.Length + 1;

      Position := (Container => Container'Unchecked_Access, Index => Idx);
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
   end Include;

   -------------
   -- Replace --
   -------------

   procedure Replace (Container : in Out Set; New_Item : Element_Type) is
      Position : Cursor := Find (Container, New_Item);
   begin
      if not Has_Element (Position) then
         raise Constraint_Error;
      end if;
      Container.Entries (Position.Index).Element := New_Item;
   end Replace;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in Out Set; Item : Element_Type) is
      Position : Cursor := Find (Container, Item);
   begin
      if not Has_Element (Position) then
         raise Constraint_Error;
      end if;
      Delete (Container, Position);
   end Delete;

   procedure Delete (Container : in Out Set; Position : in Out Cursor) is
   begin
      if not Has_Element (Position) then
         raise Constraint_Error;
      end if;

      Container.Entries (Position.Index).State := Deleted;
      Container.Length := Container.Length - 1;
      Position := No_Element;
   end Delete;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Container : in Out Set; Item : Element_Type) is
      Position : Cursor := Find (Container, Item);
   begin
      if Has_Element (Position) then
         Delete (Container, Position);
      end if;
   end Exclude;

   -----------
   -- Union --
   -----------

   procedure Union (Target : in Out Set; Source : Set) is
   begin
      for I in Bucket_Index loop
         if Source.Entries (I).State = Occupied then
            Include (Target, Source.Entries (I).Element);
         end if;
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
      for I in Bucket_Index loop
         if Target.Entries (I).State = Occupied then
            if Contains (Source, Target.Entries (I).Element) then
               Include (Result, Target.Entries (I).Element);
            end if;
         end if;
      end loop;
      Target := Result;
   end Intersection;

   function Intersection (Left, Right : Set) return Set is
      Result : Set;
   begin
      for I in Bucket_Index loop
         if Left.Entries (I).State = Occupied then
            if Contains (Right, Left.Entries (I).Element) then
               Include (Result, Left.Entries (I).Element);
            end if;
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
      for I in Bucket_Index loop
         if Target.Entries (I).State = Occupied then
            if not Contains (Source, Target.Entries (I).Element) then
               Include (Result, Target.Entries (I).Element);
            end if;
         end if;
      end loop;
      Target := Result;
   end Difference;

   function Difference (Left, Right : Set) return Set is
      Result : Set;
   begin
      for I in Bucket_Index loop
         if Left.Entries (I).State = Occupied then
            if not Contains (Right, Left.Entries (I).Element) then
               Include (Result, Left.Entries (I).Element);
            end if;
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
      for I in Bucket_Index loop
         if Target.Entries (I).State = Occupied then
            if not Contains (Source, Target.Entries (I).Element) then
               Include (Result, Target.Entries (I).Element);
            end if;
         end if;
      end loop;

      -- Elements in Source but not in Target
      for I in Bucket_Index loop
         if Source.Entries (I).State = Occupied then
            if not Contains (Target, Source.Entries (I).Element) then
               Include (Result, Source.Entries (I).Element);
            end if;
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
      for I in Bucket_Index loop
         if Subset.Entries (I).State = Occupied then
            if not Contains (Of_Set, Subset.Entries (I).Element) then
               return False;
            end if;
         end if;
      end loop;
      return True;
   end Is_Subset;

   -------------
   -- Overlap --
   -------------

   function Overlap (Left, Right : Set) return Boolean is
   begin
      for I in Bucket_Index loop
         if Left.Entries (I).State = Occupied then
            if Contains (Right, Left.Entries (I).Element) then
               return True;
            end if;
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
      for I in Bucket_Index loop
         if Container.Entries (I).State = Occupied then
            Process ((Container => Container'Unchecked_Access, Index => I));
         end if;
      end loop;
   end Iterate;

end Ada.Containers.Hashed_Sets;
