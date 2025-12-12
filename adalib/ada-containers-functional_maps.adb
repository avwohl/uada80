-- Ada.Containers.Functional_Maps body for Z80
-- Functional maps implementation

package body Ada.Containers.Functional_Maps is

   function Find (Container : Map; Key : Key_Type) return Natural is
   begin
      for I in 1 .. Max_Elements loop
         if Container.Entries (I).Used and then Container.Entries (I).Key = Key then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   function Find_Free (Container : Map) return Natural is
   begin
      for I in 1 .. Max_Elements loop
         if not Container.Entries (I).Used then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Free;

   ------------
   -- Length --
   ------------

   function Length (Container : Map) return Count_Type is
   begin
      return Container.Count;
   end Length;

   -------------
   -- Has_Key --
   -------------

   function Has_Key (Container : Map; Key : Key_Type) return Boolean is
   begin
      return Find (Container, Key) > 0;
   end Has_Key;

   ---------
   -- Get --
   ---------

   function Get (Container : Map; Key : Key_Type) return Element_Type is
      Idx : constant Natural := Find (Container, Key);
   begin
      return Container.Entries (Idx).Value;
   end Get;

   ---------
   -- Set --
   ---------

   function Set
     (Container : Map;
      Key       : Key_Type;
      New_Item  : Element_Type) return Map
   is
      Result : Map := Container;
      Idx    : Natural := Find (Container, Key);
   begin
      if Idx > 0 then
         Result.Entries (Idx).Value := New_Item;
      else
         Idx := Find_Free (Result);
         if Idx > 0 then
            Result.Entries (Idx) := (Key => Key, Value => New_Item, Used => True);
            Result.Count := Result.Count + 1;
         end if;
      end if;
      return Result;
   end Set;

   ---------
   -- Add --
   ---------

   function Add
     (Container : Map;
      Key       : Key_Type;
      New_Item  : Element_Type) return Map
   is
   begin
      return Set (Container, Key, New_Item);
   end Add;

   ------------
   -- Remove --
   ------------

   function Remove (Container : Map; Key : Key_Type) return Map is
      Result : Map := Container;
      Idx    : constant Natural := Find (Container, Key);
   begin
      if Idx > 0 then
         Result.Entries (Idx).Used := False;
         Result.Count := Result.Count - 1;
      end if;
      return Result;
   end Remove;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Map) return Boolean is
   begin
      if Left.Count /= Right.Count then
         return False;
      end if;

      for I in 1 .. Max_Elements loop
         if Left.Entries (I).Used then
            declare
               Idx : constant Natural := Find (Right, Left.Entries (I).Key);
            begin
               if Idx = 0 or else
                  Right.Entries (Idx).Value /= Left.Entries (I).Value
               then
                  return False;
               end if;
            end;
         end if;
      end loop;

      return True;
   end "=";

end Ada.Containers.Functional_Maps;
