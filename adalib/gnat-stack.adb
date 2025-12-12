-- GNAT.Stack body for Z80
-- Generic stack data structure implementation

package body GNAT.Stack is

   package body Bounded is

      -----------
      -- Clear --
      -----------

      procedure Clear (S : out Stack) is
      begin
         S.Count := 0;
      end Clear;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty (S : Stack) return Boolean is
      begin
         return S.Count = 0;
      end Is_Empty;

      -------------
      -- Is_Full --
      -------------

      function Is_Full (S : Stack) return Boolean is
      begin
         return S.Count >= Max_Size;
      end Is_Full;

      ----------
      -- Size --
      ----------

      function Size (S : Stack) return Natural is
      begin
         return S.Count;
      end Size;

      ----------
      -- Push --
      ----------

      procedure Push (S : in Out Stack; Item : Element_Type) is
      begin
         if S.Count >= Max_Size then
            raise Constraint_Error with "stack overflow";
         end if;
         S.Count := S.Count + 1;
         S.Data (S.Count) := Item;
      end Push;

      ---------
      -- Pop --
      ---------

      procedure Pop (S : in Out Stack; Item : out Element_Type) is
      begin
         if S.Count = 0 then
            raise Constraint_Error with "stack underflow";
         end if;
         Item := S.Data (S.Count);
         S.Count := S.Count - 1;
      end Pop;

      ---------
      -- Top --
      ---------

      function Top (S : Stack) return Element_Type is
      begin
         if S.Count = 0 then
            raise Constraint_Error with "stack is empty";
         end if;
         return S.Data (S.Count);
      end Top;

      ----------
      -- Peek --
      ----------

      function Peek (S : Stack; Depth : Positive := 1) return Element_Type is
      begin
         if Depth > S.Count then
            raise Constraint_Error with "depth exceeds stack size";
         end if;
         return S.Data (S.Count - Depth + 1);
      end Peek;

   end Bounded;

end GNAT.Stack;
