-- GNAT.Sets body for Z80
-- Generic set container implementation

package body GNAT.Sets is

   function Find (S : Set; Element : Element_Type) return Natural is
   begin
      for I in 1 .. S.Count loop
         if S.Elements (I) = Element then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   ------------
   -- Create --
   ------------

   function Create return Set is
   begin
      return (Elements => <>, Count => 0);
   end Create;

   ------------
   -- Insert --
   ------------

   procedure Insert (S : in Out Set; Element : Element_Type) is
   begin
      if Find (S, Element) = 0 then
         if S.Count < Max_Elements then
            S.Count := S.Count + 1;
            S.Elements (S.Count) := Element;
         end if;
      end if;
   end Insert;

   ------------
   -- Delete --
   ------------

   procedure Delete (S : in Out Set; Element : Element_Type) is
      Idx : constant Natural := Find (S, Element);
   begin
      if Idx > 0 then
         S.Elements (Idx) := S.Elements (S.Count);
         S.Count := S.Count - 1;
      end if;
   end Delete;

   --------------
   -- Contains --
   --------------

   function Contains (S : Set; Element : Element_Type) return Boolean is
   begin
      return Find (S, Element) > 0;
   end Contains;

   ----------
   -- Size --
   ----------

   function Size (S : Set) return Natural is
   begin
      return S.Count;
   end Size;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (S : Set) return Boolean is
   begin
      return S.Count = 0;
   end Is_Empty;

   -----------
   -- Clear --
   -----------

   procedure Clear (S : in Out Set) is
   begin
      S.Count := 0;
   end Clear;

   -----------
   -- Union --
   -----------

   function Union (Left, Right : Set) return Set is
      Result : Set := Left;
   begin
      for I in 1 .. Right.Count loop
         Insert (Result, Right.Elements (I));
      end loop;
      return Result;
   end Union;

   ------------------
   -- Intersection --
   ------------------

   function Intersection (Left, Right : Set) return Set is
      Result : Set;
   begin
      for I in 1 .. Left.Count loop
         if Contains (Right, Left.Elements (I)) then
            Insert (Result, Left.Elements (I));
         end if;
      end loop;
      return Result;
   end Intersection;

   ----------------
   -- Difference --
   ----------------

   function Difference (Left, Right : Set) return Set is
      Result : Set;
   begin
      for I in 1 .. Left.Count loop
         if not Contains (Right, Left.Elements (I)) then
            Insert (Result, Left.Elements (I));
         end if;
      end loop;
      return Result;
   end Difference;

   ---------------
   -- Is_Subset --
   ---------------

   function Is_Subset (Left, Right : Set) return Boolean is
   begin
      for I in 1 .. Left.Count loop
         if not Contains (Right, Left.Elements (I)) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Subset;

   -----------
   -- First --
   -----------

   function First (S : Set) return Cursor is
   begin
      if S.Count > 0 then
         return (Container => S'Unrestricted_Access, Index => 1);
      else
         return (Container => null, Index => 0);
      end if;
   end First;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Index < Position.Container.Count then
         return (Position.Container, Position.Index + 1);
      else
         return (null, 0);
      end if;
   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Index > 0;
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Container.Elements (Position.Index);
   end Element;

end GNAT.Sets;
