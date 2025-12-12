-- Ada.Containers.Hashed_Maps body for Z80
-- Generic hash-based key-value map implementation

package body Ada.Containers.Hashed_Maps is

   -- Internal: Calculate bucket index from hash
   function Get_Bucket (Key : Key_Type) return Bucket_Index is
   begin
      return Bucket_Index (Hash (Key) mod Hash_Type (Table_Size));
   end Get_Bucket;

   -- Internal: Find bucket for key using linear probing
   function Find_Bucket
     (Container : Map;
      Key       : Key_Type;
      For_Insert : Boolean := False) return Bucket_Index
   is
      Start_Idx    : Bucket_Index := Get_Bucket (Key);
      Idx          : Bucket_Index := Start_Idx;
      First_Deleted : Bucket_Index := Bucket_Index'Last;
      Found_Deleted : Boolean := False;
   begin
      loop
         case Container.Entries (Idx).State is
            when Empty =>
               -- Empty slot found
               if For_Insert and Found_Deleted then
                  return First_Deleted;
               end if;
               return Idx;

            when Deleted =>
               -- Remember first deleted slot for insertion
               if For_Insert and not Found_Deleted then
                  First_Deleted := Idx;
                  Found_Deleted := True;
               end if;

            when Occupied =>
               -- Check if this is the key we're looking for
               if Equivalent_Keys (Container.Entries (Idx).Key, Key) then
                  return Idx;
               end if;
         end case;

         -- Linear probing
         Idx := (Idx + 1) mod Table_Size;

         -- Full circle - table is full
         exit when Idx = Start_Idx;
      end loop;

      -- Table full or key not found
      if For_Insert and Found_Deleted then
         return First_Deleted;
      end if;

      return Start_Idx;  -- Return start index as indicator
   end Find_Bucket;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Map) return Boolean is
      L_Cursor : Cursor := First (Left);
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      while Has_Element (L_Cursor) loop
         declare
            R_Cursor : Cursor := Find (Right, Key (L_Cursor));
         begin
            if not Has_Element (R_Cursor) then
               return False;
            end if;
            if Element (L_Cursor) /= Element (R_Cursor) then
               return False;
            end if;
         end;
         Next (L_Cursor);
      end loop;

      return True;
   end "=";

   --------------
   -- Capacity --
   --------------

   function Capacity (Container : Map) return Count_Type is
      pragma Unreferenced (Container);
   begin
      return Count_Type (Table_Size);
   end Capacity;

   ----------------------
   -- Reserve_Capacity --
   ----------------------

   procedure Reserve_Capacity (Container : in Out Map; Capacity : Count_Type) is
      pragma Unreferenced (Container);
      pragma Unreferenced (Capacity);
   begin
      -- Fixed size table, no dynamic resizing on Z80
      null;
   end Reserve_Capacity;

   ------------
   -- Length --
   ------------

   function Length (Container : Map) return Count_Type is
   begin
      return Container.Length;
   end Length;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Map) return Boolean is
   begin
      return Container.Length = 0;
   end Is_Empty;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in Out Map) is
   begin
      for I in Bucket_Index loop
         Container.Entries (I).State := Empty;
      end loop;
      Container.Length := 0;
   end Clear;

   ---------
   -- Key --
   ---------

   function Key (Position : Cursor) return Key_Type is
   begin
      if Position.Container = null or else
         Position.Container.Entries (Position.Index).State /= Occupied
      then
         raise Constraint_Error;
      end if;
      return Position.Container.Entries (Position.Index).Key;
   end Key;

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

   function Element (Container : Map; Key : Key_Type) return Element_Type is
      Pos : Cursor := Find (Container, Key);
   begin
      if not Has_Element (Pos) then
         raise Constraint_Error;
      end if;
      return Element (Pos);
   end Element;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in Out Map;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      if Container.Entries (Position.Index).State /= Occupied then
         raise Constraint_Error;
      end if;
      Container.Entries (Position.Index).Element := New_Item;
   end Replace_Element;

   -----------
   -- First --
   -----------

   function First (Container : Map) return Cursor is
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

   function Find (Container : Map; Key : Key_Type) return Cursor is
      Idx : Bucket_Index := Find_Bucket (Container, Key);
   begin
      if Container.Entries (Idx).State = Occupied and then
         Equivalent_Keys (Container.Entries (Idx).Key, Key)
      then
         return (Container => Container'Unchecked_Access, Index => Idx);
      end if;
      return No_Element;
   end Find;

   --------------
   -- Contains --
   --------------

   function Contains (Container : Map; Key : Key_Type) return Boolean is
   begin
      return Has_Element (Find (Container, Key));
   end Contains;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in Out Map;
      Key       : Key_Type;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean)
   is
      Idx : Bucket_Index;
   begin
      if Container.Length >= Count_Type (Table_Size) - 1 then
         -- Keep at least one empty slot for probing
         raise Capacity_Error;
      end if;

      Idx := Find_Bucket (Container, Key, For_Insert => True);

      if Container.Entries (Idx).State = Occupied and then
         Equivalent_Keys (Container.Entries (Idx).Key, Key)
      then
         -- Key already exists
         Position := (Container => Container'Unchecked_Access, Index => Idx);
         Inserted := False;
         return;
      end if;

      -- Insert new entry
      Container.Entries (Idx) := (Key => Key, Element => New_Item, State => Occupied);
      Container.Length := Container.Length + 1;

      Position := (Container => Container'Unchecked_Access, Index => Idx);
      Inserted := True;
   end Insert;

   procedure Insert
     (Container : in Out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   is
      Position : Cursor;
      Inserted : Boolean;
   begin
      Insert (Container, Key, New_Item, Position, Inserted);
      if not Inserted then
         raise Constraint_Error;
      end if;
   end Insert;

   -------------
   -- Include --
   -------------

   procedure Include
     (Container : in Out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   is
      Position : Cursor;
      Inserted : Boolean;
   begin
      Insert (Container, Key, New_Item, Position, Inserted);
      if not Inserted then
         Container.Entries (Position.Index).Element := New_Item;
      end if;
   end Include;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Container : in Out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   is
      Position : Cursor := Find (Container, Key);
   begin
      if not Has_Element (Position) then
         raise Constraint_Error;
      end if;
      Container.Entries (Position.Index).Element := New_Item;
   end Replace;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in Out Map; Key : Key_Type) is
      Position : Cursor := Find (Container, Key);
   begin
      if not Has_Element (Position) then
         raise Constraint_Error;
      end if;
      Delete (Container, Position);
   end Delete;

   procedure Delete (Container : in Out Map; Position : in Out Cursor) is
   begin
      if not Has_Element (Position) then
         raise Constraint_Error;
      end if;

      -- Mark as deleted (tombstone) for linear probing to work correctly
      Container.Entries (Position.Index).State := Deleted;
      Container.Length := Container.Length - 1;
      Position := No_Element;
   end Delete;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Container : in Out Map; Key : Key_Type) is
      Position : Cursor := Find (Container, Key);
   begin
      if Has_Element (Position) then
         Delete (Container, Position);
      end if;
   end Exclude;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor))
   is
   begin
      for I in Bucket_Index loop
         if Container.Entries (I).State = Occupied then
            Process ((Container => Container'Unchecked_Access, Index => I));
         end if;
      end loop;
   end Iterate;

end Ada.Containers.Hashed_Maps;
