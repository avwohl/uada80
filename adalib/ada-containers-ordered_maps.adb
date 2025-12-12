-- Ada.Containers.Ordered_Maps body for Z80
-- Generic ordered key-value map implementation

package body Ada.Containers.Ordered_Maps is

   -- Internal: Binary search for key position
   -- Returns index where key is or should be inserted
   function Find_Position (Container : Map; Key : Key_Type) return Entry_Index is
      Low   : Entry_Index := 1;
      High  : Entry_Index := Entry_Index (Container.Length);
      Mid   : Entry_Index;
   begin
      if Container.Length = 0 then
         return 1;
      end if;

      while Low <= High loop
         Mid := (Low + High) / 2;

         if Container.Entries (Mid).Key = Key then
            return Mid;
         elsif Container.Entries (Mid).Key < Key then
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

   function "=" (Left, Right : Map) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      for I in 1 .. Entry_Index (Left.Length) loop
         if Left.Entries (I).Key /= Right.Entries (I).Key then
            return False;
         end if;
         -- Note: Element comparison would require "=" for Element_Type
      end loop;

      return True;
   end "=";

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
      for I in 1 .. Max_Entries loop
         Container.Entries (I).Used := False;
      end loop;
      Container.Length := 0;
   end Clear;

   ---------
   -- Key --
   ---------

   function Key (Position : Cursor) return Key_Type is
   begin
      if Position.Index = Null_Entry or Position.Container = null then
         raise Constraint_Error;
      end if;
      return Position.Container.Entries (Position.Index).Key;
   end Key;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position.Index = Null_Entry or Position.Container = null then
         raise Constraint_Error;
      end if;
      return Position.Container.Entries (Position.Index).Element;
   end Element;

   function Element (Container : Map; Key : Key_Type) return Element_Type is
      Pos : Cursor := Find (Container, Key);
   begin
      if Pos = No_Element then
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
      if Position.Index = Null_Entry then
         raise Constraint_Error;
      end if;
      Container.Entries (Position.Index).Element := New_Item;
   end Replace_Element;

   -----------
   -- First --
   -----------

   function First (Container : Map) return Cursor is
   begin
      if Container.Length = 0 then
         return No_Element;
      end if;
      return (Container => Container'Unchecked_Access, Index => 1);
   end First;

   ----------
   -- Last --
   ----------

   function Last (Container : Map) return Cursor is
   begin
      if Container.Length = 0 then
         return No_Element;
      end if;
      return (Container => Container'Unchecked_Access, Index => Entry_Index (Container.Length));
   end Last;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Index = Null_Entry or Position.Container = null then
         return No_Element;
      end if;

      if Position.Index >= Entry_Index (Position.Container.Length) then
         return No_Element;
      end if;

      return (Container => Position.Container, Index => Position.Index + 1);
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous (Position : Cursor) return Cursor is
   begin
      if Position.Index = Null_Entry or Position.Container = null then
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
      return Position.Index /= Null_Entry and Position.Container /= null;
   end Has_Element;

   ----------
   -- Find --
   ----------

   function Find (Container : Map; Key : Key_Type) return Cursor is
      Pos : Entry_Index;
   begin
      if Container.Length = 0 then
         return No_Element;
      end if;

      Pos := Find_Position (Container, Key);

      if Pos <= Entry_Index (Container.Length) and then
         Container.Entries (Pos).Key = Key
      then
         return (Container => Container'Unchecked_Access, Index => Pos);
      end if;

      return No_Element;
   end Find;

   --------------
   -- Contains --
   --------------

   function Contains (Container : Map; Key : Key_Type) return Boolean is
   begin
      return Find (Container, Key) /= No_Element;
   end Contains;

   -----------
   -- Floor --
   -----------

   function Floor (Container : Map; Key : Key_Type) return Cursor is
      Pos : Entry_Index;
   begin
      if Container.Length = 0 then
         return No_Element;
      end if;

      Pos := Find_Position (Container, Key);

      -- If exact match
      if Pos <= Entry_Index (Container.Length) and then
         Container.Entries (Pos).Key = Key
      then
         return (Container => Container'Unchecked_Access, Index => Pos);
      end if;

      -- Return previous (largest key <= given key)
      if Pos > 1 then
         return (Container => Container'Unchecked_Access, Index => Pos - 1);
      end if;

      return No_Element;
   end Floor;

   -------------
   -- Ceiling --
   -------------

   function Ceiling (Container : Map; Key : Key_Type) return Cursor is
      Pos : Entry_Index;
   begin
      if Container.Length = 0 then
         return No_Element;
      end if;

      Pos := Find_Position (Container, Key);

      if Pos <= Entry_Index (Container.Length) then
         return (Container => Container'Unchecked_Access, Index => Pos);
      end if;

      return No_Element;
   end Ceiling;

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
      Pos : Entry_Index;
   begin
      if Container.Length >= Count_Type (Max_Entries) then
         raise Capacity_Error;
      end if;

      Pos := Find_Position (Container, Key);

      -- Check for duplicate
      if Pos <= Entry_Index (Container.Length) and then
         Container.Entries (Pos).Key = Key
      then
         Position := (Container => Container'Unchecked_Access, Index => Pos);
         Inserted := False;
         return;
      end if;

      -- Shift elements to make room
      for I in reverse Pos .. Entry_Index (Container.Length) loop
         Container.Entries (I + 1) := Container.Entries (I);
      end loop;

      -- Insert new entry
      Container.Entries (Pos) := (Key => Key, Element => New_Item, Used => True);
      Container.Length := Container.Length + 1;

      Position := (Container => Container'Unchecked_Access, Index => Pos);
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
         raise Constraint_Error;  -- Key already exists
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
         -- Key exists, replace element
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
      if Position = No_Element then
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
      if Position = No_Element then
         raise Constraint_Error;
      end if;
      Delete (Container, Position);
   end Delete;

   procedure Delete (Container : in Out Map; Position : in Out Cursor) is
      Idx : Entry_Index := Position.Index;
   begin
      if Position.Index = Null_Entry then
         raise Constraint_Error;
      end if;

      -- Shift remaining elements down
      for I in Idx .. Entry_Index (Container.Length) - 1 loop
         Container.Entries (I) := Container.Entries (I + 1);
      end loop;

      Container.Entries (Entry_Index (Container.Length)).Used := False;
      Container.Length := Container.Length - 1;
      Position := No_Element;
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (Container : in Out Map) is
      Position : Cursor := First (Container);
   begin
      if Position /= No_Element then
         Delete (Container, Position);
      end if;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Container : in Out Map) is
   begin
      if Container.Length > 0 then
         Container.Entries (Entry_Index (Container.Length)).Used := False;
         Container.Length := Container.Length - 1;
      end if;
   end Delete_Last;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Container : in Out Map; Key : Key_Type) is
      Position : Cursor := Find (Container, Key);
   begin
      if Position /= No_Element then
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
      for I in 1 .. Entry_Index (Container.Length) loop
         Process ((Container => Container'Unchecked_Access, Index => I));
      end loop;
   end Iterate;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (Container : Map;
      Process   : not null access procedure (Position : Cursor))
   is
   begin
      for I in reverse 1 .. Entry_Index (Container.Length) loop
         Process ((Container => Container'Unchecked_Access, Index => I));
      end loop;
   end Reverse_Iterate;

end Ada.Containers.Ordered_Maps;
