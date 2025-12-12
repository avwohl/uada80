-- Ada.Containers.Indefinite_Ordered_Maps body for Z80
-- Generic ordered map for indefinite types implementation

with Ada.Unchecked_Deallocation;

package body Ada.Containers.Indefinite_Ordered_Maps is

   procedure Free_Key is new Ada.Unchecked_Deallocation
     (Key_Type, Key_Access);
   procedure Free_Element is new Ada.Unchecked_Deallocation
     (Element_Type, Element_Access);

   function Equivalent (Left, Right : Key_Type) return Boolean is
   begin
      return not (Left < Right) and not (Right < Left);
   end Equivalent;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Map) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      for I in 1 .. Left.Length loop
         if not Equivalent (Left.Entries (I).Key.all, Right.Entries (I).Key.all) then
            return False;
         end if;
         if Left.Entries (I).Element.all /= Right.Entries (I).Element.all then
            return False;
         end if;
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
      for I in 1 .. Container.Length loop
         if Container.Entries (I).Key /= null then
            Free_Key (Container.Entries (I).Key);
         end if;
         if Container.Entries (I).Element /= null then
            Free_Element (Container.Entries (I).Element);
         end if;
      end loop;
      Container.Length := 0;
   end Clear;

   ---------
   -- Key --
   ---------

   function Key (Position : Cursor) return Key_Type is
   begin
      if Position.Index = 0 then
         raise Constraint_Error;
      end if;
      return Position.Container.Entries (Position.Index).Key.all;
   end Key;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position.Index = 0 then
         raise Constraint_Error;
      end if;
      return Position.Container.Entries (Position.Index).Element.all;
   end Element;

   function Element (Container : Map; Key : Key_Type) return Element_Type is
      Pos : constant Cursor := Find (Container, Key);
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
      if Position.Index = 0 then
         raise Constraint_Error;
      end if;
      if Container.Entries (Position.Index).Element /= null then
         Free_Element (Container.Entries (Position.Index).Element);
      end if;
      Container.Entries (Position.Index).Element := new Element_Type'(New_Item);
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

   function Find (Container : Map; Key : Key_Type) return Cursor is
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
            if Equivalent (Container.Entries (Mid).Key.all, Key) then
               return (Container => Container'Unchecked_Access, Index => Mid);
            elsif Container.Entries (Mid).Key.all < Key then
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

   function Contains (Container : Map; Key : Key_Type) return Boolean is
   begin
      return Find (Container, Key) /= No_Element;
   end Contains;

   -----------
   -- Floor --
   -----------

   function Floor (Container : Map; Key : Key_Type) return Cursor is
      Result : Cursor := No_Element;
   begin
      for I in 1 .. Container.Length loop
         if Container.Entries (I).Key.all < Key or
            Equivalent (Container.Entries (I).Key.all, Key)
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

   function Ceiling (Container : Map; Key : Key_Type) return Cursor is
   begin
      for I in 1 .. Container.Length loop
         if not (Container.Entries (I).Key.all < Key) then
            return (Container => Container'Unchecked_Access, Index => I);
         end if;
      end loop;
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
      Insert_Pos : Count_Type := Container.Length + 1;
   begin
      for I in 1 .. Container.Length loop
         if Equivalent (Container.Entries (I).Key.all, Key) then
            Position := (Container => Container'Unchecked_Access, Index => I);
            Inserted := False;
            return;
         elsif Key < Container.Entries (I).Key.all then
            Insert_Pos := I;
            exit;
         end if;
      end loop;

      if Container.Length >= Max_Entries then
         raise Capacity_Error;
      end if;

      for I in reverse Insert_Pos .. Container.Length loop
         Container.Entries (I + 1) := Container.Entries (I);
      end loop;

      Container.Entries (Insert_Pos).Key := new Key_Type'(Key);
      Container.Entries (Insert_Pos).Element := new Element_Type'(New_Item);
      Container.Length := Container.Length + 1;

      Position := (Container => Container'Unchecked_Access, Index => Insert_Pos);
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
         Replace_Element (Container, Position, New_Item);
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
      Position : constant Cursor := Find (Container, Key);
   begin
      if Position = No_Element then
         raise Constraint_Error;
      end if;
      Replace_Element (Container, Position, New_Item);
   end Replace;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (Container : in Out Map; Key : Key_Type) is
      Position : constant Cursor := Find (Container, Key);
   begin
      if Position /= No_Element then
         if Container.Entries (Position.Index).Key /= null then
            Free_Key (Container.Entries (Position.Index).Key);
         end if;
         if Container.Entries (Position.Index).Element /= null then
            Free_Element (Container.Entries (Position.Index).Element);
         end if;
         for I in Position.Index .. Container.Length - 1 loop
            Container.Entries (I) := Container.Entries (I + 1);
         end loop;
         Container.Entries (Container.Length) := (Key => null, Element => null);
         Container.Length := Container.Length - 1;
      end if;
   end Exclude;

   ------------
   -- Delete --
   ------------

   procedure Delete (Container : in Out Map; Key : Key_Type) is
   begin
      if not Contains (Container, Key) then
         raise Constraint_Error;
      end if;
      Exclude (Container, Key);
   end Delete;

   procedure Delete (Container : in Out Map; Position : in Out Cursor) is
   begin
      if Position.Index = 0 then
         raise Constraint_Error;
      end if;
      Delete (Container, Container.Entries (Position.Index).Key.all);
      Position := No_Element;
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (Container : in Out Map) is
   begin
      if Container.Length > 0 then
         if Container.Entries (1).Key /= null then
            Free_Key (Container.Entries (1).Key);
         end if;
         if Container.Entries (1).Element /= null then
            Free_Element (Container.Entries (1).Element);
         end if;
         for I in 1 .. Container.Length - 1 loop
            Container.Entries (I) := Container.Entries (I + 1);
         end loop;
         Container.Entries (Container.Length) := (Key => null, Element => null);
         Container.Length := Container.Length - 1;
      end if;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Container : in Out Map) is
   begin
      if Container.Length > 0 then
         if Container.Entries (Container.Length).Key /= null then
            Free_Key (Container.Entries (Container.Length).Key);
         end if;
         if Container.Entries (Container.Length).Element /= null then
            Free_Element (Container.Entries (Container.Length).Element);
         end if;
         Container.Entries (Container.Length) := (Key => null, Element => null);
         Container.Length := Container.Length - 1;
      end if;
   end Delete_Last;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Map;
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
     (Container : Map;
      Process   : not null access procedure (Position : Cursor))
   is
   begin
      for I in reverse 1 .. Container.Length loop
         Process ((Container => Container'Unchecked_Access, Index => I));
      end loop;
   end Reverse_Iterate;

end Ada.Containers.Indefinite_Ordered_Maps;
