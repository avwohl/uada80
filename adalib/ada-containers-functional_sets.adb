-- Ada.Containers.Functional_Sets body for Z80
-- Functional sets implementation

package body Ada.Containers.Functional_Sets is

   function Find (Container : Set; Item : Element_Type) return Natural is
   begin
      for I in 1 .. Max_Elements loop
         if Container.Used (I) and then Container.Elements (I) = Item then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   function Find_Free (Container : Set) return Natural is
   begin
      for I in 1 .. Max_Elements loop
         if not Container.Used (I) then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Free;

   ------------
   -- Length --
   ------------

   function Length (Container : Set) return Count_Type is
   begin
      return Container.Count;
   end Length;

   --------------
   -- Contains --
   --------------

   function Contains (Container : Set; Item : Element_Type) return Boolean is
   begin
      return Find (Container, Item) > 0;
   end Contains;

   ---------
   -- Add --
   ---------

   function Add (Container : Set; Item : Element_Type) return Set is
      Result : Set := Container;
      Idx    : Natural;
   begin
      if Find (Container, Item) = 0 then
         Idx := Find_Free (Result);
         if Idx > 0 then
            Result.Elements (Idx) := Item;
            Result.Used (Idx) := True;
            Result.Count := Result.Count + 1;
         end if;
      end if;
      return Result;
   end Add;

   ------------
   -- Remove --
   ------------

   function Remove (Container : Set; Item : Element_Type) return Set is
      Result : Set := Container;
      Idx    : constant Natural := Find (Container, Item);
   begin
      if Idx > 0 then
         Result.Used (Idx) := False;
         Result.Count := Result.Count - 1;
      end if;
      return Result;
   end Remove;

   -----------
   -- Union --
   -----------

   function Union (Left, Right : Set) return Set is
      Result : Set := Left;
   begin
      for I in 1 .. Max_Elements loop
         if Right.Used (I) then
            Result := Add (Result, Right.Elements (I));
         end if;
      end loop;
      return Result;
   end Union;

   ------------------
   -- Intersection --
   ------------------

   function Intersection (Left, Right : Set) return Set is
      Result : Set := Empty_Set;
   begin
      for I in 1 .. Max_Elements loop
         if Left.Used (I) and then Contains (Right, Left.Elements (I)) then
            Result := Add (Result, Left.Elements (I));
         end if;
      end loop;
      return Result;
   end Intersection;

   ----------------
   -- Difference --
   ----------------

   function Difference (Left, Right : Set) return Set is
      Result : Set := Empty_Set;
   begin
      for I in 1 .. Max_Elements loop
         if Left.Used (I) and then not Contains (Right, Left.Elements (I)) then
            Result := Add (Result, Left.Elements (I));
         end if;
      end loop;
      return Result;
   end Difference;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Set) return Boolean is
   begin
      if Left.Count /= Right.Count then
         return False;
      end if;

      for I in 1 .. Max_Elements loop
         if Left.Used (I) and then not Contains (Right, Left.Elements (I)) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   ---------------
   -- Is_Subset --
   ---------------

   function Is_Subset (Left, Right : Set) return Boolean is
   begin
      for I in 1 .. Max_Elements loop
         if Left.Used (I) and then not Contains (Right, Left.Elements (I)) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Subset;

end Ada.Containers.Functional_Sets;
