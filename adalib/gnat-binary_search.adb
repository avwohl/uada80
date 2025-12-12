-- GNAT.Binary_Search body for Z80
-- Generic binary search utilities implementation

package body GNAT.Binary_Search is

   ----------
   -- Find --
   ----------

   function Find
     (A       : Array_Type;
      Element : Element_Type) return Index_Type
   is
      Lo : Index_Type := A'First;
      Hi : Index_Type := A'Last;
      Mid : Index_Type;
   begin
      while Lo <= Hi loop
         Mid := Index_Type'Val ((Index_Type'Pos (Lo) + Index_Type'Pos (Hi)) / 2);
         if A (Mid) < Element then
            Lo := Index_Type'Succ (Mid);
         elsif Element < A (Mid) then
            Hi := Index_Type'Pred (Mid);
         else
            return Mid;
         end if;
      end loop;
      raise Constraint_Error with "element not found";
   end Find;

   ---------------------
   -- Find_Or_Nothing --
   ---------------------

   function Find_Or_Nothing
     (A       : Array_Type;
      Element : Element_Type) return Natural
   is
      Lo  : Integer := Index_Type'Pos (A'First);
      Hi  : Integer := Index_Type'Pos (A'Last);
      Mid : Integer;
   begin
      while Lo <= Hi loop
         Mid := (Lo + Hi) / 2;
         declare
            M : constant Index_Type := Index_Type'Val (Mid);
         begin
            if A (M) < Element then
               Lo := Mid + 1;
            elsif Element < A (M) then
               Hi := Mid - 1;
            else
               return Mid;
            end if;
         end;
      end loop;
      return 0;
   end Find_Or_Nothing;

   -----------------
   -- Lower_Bound --
   -----------------

   function Lower_Bound
     (A       : Array_Type;
      Element : Element_Type) return Index_Type
   is
      Lo  : Integer := Index_Type'Pos (A'First);
      Hi  : Integer := Index_Type'Pos (A'Last) + 1;
      Mid : Integer;
   begin
      while Lo < Hi loop
         Mid := (Lo + Hi) / 2;
         if A (Index_Type'Val (Mid)) < Element then
            Lo := Mid + 1;
         else
            Hi := Mid;
         end if;
      end loop;
      return Index_Type'Val (Lo);
   end Lower_Bound;

   -----------------
   -- Upper_Bound --
   -----------------

   function Upper_Bound
     (A       : Array_Type;
      Element : Element_Type) return Index_Type
   is
      Lo  : Integer := Index_Type'Pos (A'First);
      Hi  : Integer := Index_Type'Pos (A'Last) + 1;
      Mid : Integer;
   begin
      while Lo < Hi loop
         Mid := (Lo + Hi) / 2;
         if not (Element < A (Index_Type'Val (Mid))) then
            Lo := Mid + 1;
         else
            Hi := Mid;
         end if;
      end loop;
      return Index_Type'Val (Lo);
   end Upper_Bound;

end GNAT.Binary_Search;
