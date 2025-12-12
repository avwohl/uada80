-- GNAT.Binary_Tree body for Z80
-- Simple binary search tree implementation

package body GNAT.Binary_Tree is

   function Compare_Keys (K1 : Key_String; L1 : Natural;
                          K2 : String) return Integer
   is
      Min_Len : constant Natural := Natural'Min (L1, K2'Length);
   begin
      for I in 1 .. Min_Len loop
         if K1 (I) < K2 (K2'First + I - 1) then
            return -1;
         elsif K1 (I) > K2 (K2'First + I - 1) then
            return 1;
         end if;
      end loop;
      if L1 < K2'Length then
         return -1;
      elsif L1 > K2'Length then
         return 1;
      else
         return 0;
      end if;
   end Compare_Keys;

   function Allocate_Node (T : in Out Tree) return Node_Index is
   begin
      for I in 1 .. Max_Nodes loop
         if not T.Nodes (I).In_Use then
            T.Nodes (I).In_Use := True;
            T.Count := T.Count + 1;
            return Node_Index (I);
         end if;
      end loop;
      return Null_Index;
   end Allocate_Node;

   procedure Free_Node (T : in Out Tree; Idx : Node_Index) is
   begin
      if Idx /= Null_Index then
         T.Nodes (Positive (Idx)).In_Use := False;
         T.Nodes (Positive (Idx)).Left := Null_Index;
         T.Nodes (Positive (Idx)).Right := Null_Index;
         T.Count := T.Count - 1;
      end if;
   end Free_Node;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : out Tree) is
   begin
      T.Nodes := (others => (Key       => (others => ASCII.NUL),
                             Key_Len   => 0,
                             Value     => (others => ASCII.NUL),
                             Value_Len => 0,
                             Left      => Null_Index,
                             Right     => Null_Index,
                             In_Use    => False));
      T.Root := Null_Index;
      T.Count := 0;
   end Initialize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (T : Tree) return Boolean is
   begin
      return T.Root = Null_Index;
   end Is_Empty;

   ----------
   -- Size --
   ----------

   function Size (T : Tree) return Natural is
   begin
      return T.Count;
   end Size;

   --------------
   -- Contains --
   --------------

   function Contains (T : Tree; Key : String) return Boolean is
      Curr : Node_Index := T.Root;
      Cmp  : Integer;
   begin
      while Curr /= Null_Index loop
         Cmp := Compare_Keys (T.Nodes (Positive (Curr)).Key,
                              T.Nodes (Positive (Curr)).Key_Len, Key);
         if Cmp = 0 then
            return True;
         elsif Cmp > 0 then
            Curr := T.Nodes (Positive (Curr)).Left;
         else
            Curr := T.Nodes (Positive (Curr)).Right;
         end if;
      end loop;
      return False;
   end Contains;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (T       : in Out Tree;
      Key     : String;
      Value   : String;
      Success : out Boolean)
   is
      New_Node : Node_Index;
      Curr     : Node_Index;
      Cmp      : Integer;
      Key_Len  : constant Natural := Natural'Min (Key'Length, Max_Key_Length);
      Val_Len  : constant Natural := Natural'Min (Value'Length, Max_Value_Length);
   begin
      New_Node := Allocate_Node (T);
      if New_Node = Null_Index then
         Success := False;
         return;
      end if;

      -- Set up new node
      declare
         N : Node renames T.Nodes (Positive (New_Node));
      begin
         N.Key := (others => ASCII.NUL);
         for I in 1 .. Key_Len loop
            N.Key (I) := Key (Key'First + I - 1);
         end loop;
         N.Key_Len := Key_Len;

         N.Value := (others => ASCII.NUL);
         for I in 1 .. Val_Len loop
            N.Value (I) := Value (Value'First + I - 1);
         end loop;
         N.Value_Len := Val_Len;
      end;

      if T.Root = Null_Index then
         T.Root := New_Node;
         Success := True;
         return;
      end if;

      Curr := T.Root;
      loop
         Cmp := Compare_Keys (T.Nodes (Positive (Curr)).Key,
                              T.Nodes (Positive (Curr)).Key_Len, Key);
         if Cmp = 0 then
            -- Duplicate key
            Free_Node (T, New_Node);
            Success := False;
            return;
         elsif Cmp > 0 then
            if T.Nodes (Positive (Curr)).Left = Null_Index then
               T.Nodes (Positive (Curr)).Left := New_Node;
               Success := True;
               return;
            end if;
            Curr := T.Nodes (Positive (Curr)).Left;
         else
            if T.Nodes (Positive (Curr)).Right = Null_Index then
               T.Nodes (Positive (Curr)).Right := New_Node;
               Success := True;
               return;
            end if;
            Curr := T.Nodes (Positive (Curr)).Right;
         end if;
      end loop;
   end Insert;

   ----------
   -- Find --
   ----------

   procedure Find
     (T     : Tree;
      Key   : String;
      Value : out String;
      Last  : out Natural;
      Found : out Boolean)
   is
      Curr : Node_Index := T.Root;
      Cmp  : Integer;
   begin
      Last := Value'First - 1;
      Found := False;

      while Curr /= Null_Index loop
         Cmp := Compare_Keys (T.Nodes (Positive (Curr)).Key,
                              T.Nodes (Positive (Curr)).Key_Len, Key);
         if Cmp = 0 then
            Found := True;
            declare
               N        : Node renames T.Nodes (Positive (Curr));
               Copy_Len : constant Natural :=
                 Natural'Min (N.Value_Len, Value'Length);
            begin
               Value (Value'First .. Value'First + Copy_Len - 1) :=
                 String (N.Value (1 .. Copy_Len));
               Last := Value'First + Copy_Len - 1;
            end;
            return;
         elsif Cmp > 0 then
            Curr := T.Nodes (Positive (Curr)).Left;
         else
            Curr := T.Nodes (Positive (Curr)).Right;
         end if;
      end loop;
   end Find;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (T       : in Out Tree;
      Key     : String;
      Success : out Boolean)
   is
      Curr   : Node_Index := T.Root;
      Parent : Node_Index := Null_Index;
      Is_Left : Boolean := False;
      Cmp    : Integer;
   begin
      Success := False;

      -- Find node to remove
      while Curr /= Null_Index loop
         Cmp := Compare_Keys (T.Nodes (Positive (Curr)).Key,
                              T.Nodes (Positive (Curr)).Key_Len, Key);
         if Cmp = 0 then
            exit;
         end if;
         Parent := Curr;
         if Cmp > 0 then
            Curr := T.Nodes (Positive (Curr)).Left;
            Is_Left := True;
         else
            Curr := T.Nodes (Positive (Curr)).Right;
            Is_Left := False;
         end if;
      end loop;

      if Curr = Null_Index then
         return;  -- Not found
      end if;

      Success := True;

      declare
         N : Node renames T.Nodes (Positive (Curr));
         Replacement : Node_Index;
      begin
         -- Case 1: No children
         if N.Left = Null_Index and N.Right = Null_Index then
            Replacement := Null_Index;
         -- Case 2: One child
         elsif N.Left = Null_Index then
            Replacement := N.Right;
         elsif N.Right = Null_Index then
            Replacement := N.Left;
         -- Case 3: Two children - find in-order successor
         else
            declare
               Succ_Parent : Node_Index := Curr;
               Succ        : Node_Index := N.Right;
            begin
               while T.Nodes (Positive (Succ)).Left /= Null_Index loop
                  Succ_Parent := Succ;
                  Succ := T.Nodes (Positive (Succ)).Left;
               end loop;

               -- Copy successor data
               N.Key := T.Nodes (Positive (Succ)).Key;
               N.Key_Len := T.Nodes (Positive (Succ)).Key_Len;
               N.Value := T.Nodes (Positive (Succ)).Value;
               N.Value_Len := T.Nodes (Positive (Succ)).Value_Len;

               -- Remove successor
               if Succ_Parent = Curr then
                  N.Right := T.Nodes (Positive (Succ)).Right;
               else
                  T.Nodes (Positive (Succ_Parent)).Left :=
                    T.Nodes (Positive (Succ)).Right;
               end if;
               Free_Node (T, Succ);
               return;
            end;
         end if;

         -- Update parent link
         if Parent = Null_Index then
            T.Root := Replacement;
         elsif Is_Left then
            T.Nodes (Positive (Parent)).Left := Replacement;
         else
            T.Nodes (Positive (Parent)).Right := Replacement;
         end if;
         Free_Node (T, Curr);
      end;
   end Remove;

   -----------
   -- Clear --
   -----------

   procedure Clear (T : out Tree) is
   begin
      Initialize (T);
   end Clear;

   -------------
   -- Minimum --
   -------------

   function Minimum (T : Tree) return String is
      Curr : Node_Index := T.Root;
   begin
      if Curr = Null_Index then
         return "";
      end if;

      while T.Nodes (Positive (Curr)).Left /= Null_Index loop
         Curr := T.Nodes (Positive (Curr)).Left;
      end loop;

      return String (T.Nodes (Positive (Curr)).Key
                     (1 .. T.Nodes (Positive (Curr)).Key_Len));
   end Minimum;

   -------------
   -- Maximum --
   -------------

   function Maximum (T : Tree) return String is
      Curr : Node_Index := T.Root;
   begin
      if Curr = Null_Index then
         return "";
      end if;

      while T.Nodes (Positive (Curr)).Right /= Null_Index loop
         Curr := T.Nodes (Positive (Curr)).Right;
      end loop;

      return String (T.Nodes (Positive (Curr)).Key
                     (1 .. T.Nodes (Positive (Curr)).Key_Len));
   end Maximum;

   ----------------------
   -- Traverse_InOrder --
   ----------------------

   procedure Traverse_InOrder (T : Tree; Visit : Visitor) is
      procedure Traverse (Idx : Node_Index) is
      begin
         if Idx = Null_Index then
            return;
         end if;

         Traverse (T.Nodes (Positive (Idx)).Left);
         Visit (String (T.Nodes (Positive (Idx)).Key
                        (1 .. T.Nodes (Positive (Idx)).Key_Len)),
                String (T.Nodes (Positive (Idx)).Value
                        (1 .. T.Nodes (Positive (Idx)).Value_Len)));
         Traverse (T.Nodes (Positive (Idx)).Right);
      end Traverse;
   begin
      Traverse (T.Root);
   end Traverse_InOrder;

   -----------------------
   -- Traverse_PreOrder --
   -----------------------

   procedure Traverse_PreOrder (T : Tree; Visit : Visitor) is
      procedure Traverse (Idx : Node_Index) is
      begin
         if Idx = Null_Index then
            return;
         end if;

         Visit (String (T.Nodes (Positive (Idx)).Key
                        (1 .. T.Nodes (Positive (Idx)).Key_Len)),
                String (T.Nodes (Positive (Idx)).Value
                        (1 .. T.Nodes (Positive (Idx)).Value_Len)));
         Traverse (T.Nodes (Positive (Idx)).Left);
         Traverse (T.Nodes (Positive (Idx)).Right);
      end Traverse;
   begin
      Traverse (T.Root);
   end Traverse_PreOrder;

   ------------------------
   -- Traverse_PostOrder --
   ------------------------

   procedure Traverse_PostOrder (T : Tree; Visit : Visitor) is
      procedure Traverse (Idx : Node_Index) is
      begin
         if Idx = Null_Index then
            return;
         end if;

         Traverse (T.Nodes (Positive (Idx)).Left);
         Traverse (T.Nodes (Positive (Idx)).Right);
         Visit (String (T.Nodes (Positive (Idx)).Key
                        (1 .. T.Nodes (Positive (Idx)).Key_Len)),
                String (T.Nodes (Positive (Idx)).Value
                        (1 .. T.Nodes (Positive (Idx)).Value_Len)));
      end Traverse;
   begin
      Traverse (T.Root);
   end Traverse_PostOrder;

end GNAT.Binary_Tree;
