-- GNAT.Trie body for Z80
-- Simple trie implementation

package body GNAT.Trie is

   function Char_Index (C : Character) return Natural is
   begin
      if C in 'a' .. 'z' then
         return Character'Pos (C) - Character'Pos ('a') + 1;
      elsif C in 'A' .. 'Z' then
         return Character'Pos (C) - Character'Pos ('A') + 1;
      else
         return 0;  -- Invalid character
      end if;
   end Char_Index;

   function Allocate_Node (T : in Out Trie) return Node_Index is
   begin
      for I in 1 .. Max_Nodes loop
         if not T.Nodes (I).In_Use then
            T.Nodes (I) := (Children => (others => Null_Node),
                           Is_End_Word => False,
                           Word_Count => 0,
                           In_Use => True);
            T.Node_Count := T.Node_Count + 1;
            return Node_Index (I);
         end if;
      end loop;
      return Null_Node;
   end Allocate_Node;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : out Trie) is
   begin
      T.Nodes := (others => (Children => (others => Null_Node),
                             Is_End_Word => False,
                             Word_Count => 0,
                             In_Use => False));
      T.Root := Null_Node;
      T.Node_Count := 0;
      T.Word_Count := 0;
   end Initialize;

   ------------
   -- Insert --
   ------------

   procedure Insert (T : in Out Trie; Key : String; Success : out Boolean) is
      Current : Node_Index;
      Idx     : Natural;
   begin
      Success := False;

      if Key'Length = 0 then
         return;
      end if;

      -- Create root if needed
      if T.Root = Null_Node then
         T.Root := Allocate_Node (T);
         if T.Root = Null_Node then
            return;
         end if;
      end if;

      Current := T.Root;

      for C of Key loop
         Idx := Char_Index (C);
         if Idx = 0 then
            return;  -- Invalid character
         end if;

         if T.Nodes (Positive (Current)).Children (Idx) = Null_Node then
            declare
               New_Node : constant Node_Index := Allocate_Node (T);
            begin
               if New_Node = Null_Node then
                  return;  -- Out of nodes
               end if;
               T.Nodes (Positive (Current)).Children (Idx) := New_Node;
            end;
         end if;

         Current := T.Nodes (Positive (Current)).Children (Idx);
         T.Nodes (Positive (Current)).Word_Count :=
           T.Nodes (Positive (Current)).Word_Count + 1;
      end loop;

      if not T.Nodes (Positive (Current)).Is_End_Word then
         T.Nodes (Positive (Current)).Is_End_Word := True;
         T.Word_Count := T.Word_Count + 1;
      end if;

      Success := True;
   end Insert;

   --------------
   -- Contains --
   --------------

   function Contains (T : Trie; Key : String) return Boolean is
      Current : Node_Index := T.Root;
      Idx     : Natural;
   begin
      if T.Root = Null_Node or Key'Length = 0 then
         return False;
      end if;

      for C of Key loop
         Idx := Char_Index (C);
         if Idx = 0 then
            return False;
         end if;

         Current := T.Nodes (Positive (Current)).Children (Idx);
         if Current = Null_Node then
            return False;
         end if;
      end loop;

      return T.Nodes (Positive (Current)).Is_End_Word;
   end Contains;

   ------------------
   -- Prefix_Count --
   ------------------

   function Prefix_Count (T : Trie; Prefix : String) return Natural is
      Current : Node_Index := T.Root;
      Idx     : Natural;
   begin
      if T.Root = Null_Node then
         return 0;
      end if;

      if Prefix'Length = 0 then
         return T.Word_Count;
      end if;

      for C of Prefix loop
         Idx := Char_Index (C);
         if Idx = 0 then
            return 0;
         end if;

         Current := T.Nodes (Positive (Current)).Children (Idx);
         if Current = Null_Node then
            return 0;
         end if;
      end loop;

      return T.Nodes (Positive (Current)).Word_Count;
   end Prefix_Count;

   ----------------
   -- Has_Prefix --
   ----------------

   function Has_Prefix (T : Trie; Prefix : String) return Boolean is
   begin
      return Prefix_Count (T, Prefix) > 0;
   end Has_Prefix;

   ------------
   -- Remove --
   ------------

   procedure Remove (T : in Out Trie; Key : String) is
      Current : Node_Index := T.Root;
      Idx     : Natural;
   begin
      if T.Root = Null_Node or Key'Length = 0 then
         return;
      end if;

      -- First check if key exists
      for C of Key loop
         Idx := Char_Index (C);
         if Idx = 0 then
            return;
         end if;

         Current := T.Nodes (Positive (Current)).Children (Idx);
         if Current = Null_Node then
            return;
         end if;
      end loop;

      if not T.Nodes (Positive (Current)).Is_End_Word then
         return;
      end if;

      -- Key exists - unmark and decrement counts
      T.Nodes (Positive (Current)).Is_End_Word := False;
      T.Word_Count := T.Word_Count - 1;

      -- Decrement word counts along path
      Current := T.Root;
      for C of Key loop
         Idx := Char_Index (C);
         Current := T.Nodes (Positive (Current)).Children (Idx);
         T.Nodes (Positive (Current)).Word_Count :=
           T.Nodes (Positive (Current)).Word_Count - 1;
      end loop;
   end Remove;

   ----------
   -- Size --
   ----------

   function Size (T : Trie) return Natural is
   begin
      return T.Word_Count;
   end Size;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (T : Trie) return Boolean is
   begin
      return T.Word_Count = 0;
   end Is_Empty;

   -----------
   -- Clear --
   -----------

   procedure Clear (T : out Trie) is
   begin
      Initialize (T);
   end Clear;

   ------------------
   -- Autocomplete --
   ------------------

   procedure Autocomplete
     (T           : Trie;
      Prefix      : String;
      Suggestions : out Suggestion_Array;
      Lengths     : out Suggestion_Lengths;
      Count       : out Natural)
   is
      Current : Node_Index := T.Root;
      Idx     : Natural;

      procedure Collect (N : Node_Index; Depth : Natural) is
      begin
         if Count >= Max_Suggestions or Depth > Max_Key_Length then
            return;
         end if;

         if N = Null_Node then
            return;
         end if;

         if T.Nodes (Positive (N)).Is_End_Word then
            Count := Count + 1;
            -- Copy prefix
            for I in 1 .. Prefix'Length loop
               Suggestions (Count) (I) := Prefix (Prefix'First + I - 1);
            end loop;
            Lengths (Count) := Prefix'Length + Depth;
         end if;

         for I in 1 .. Alphabet_Size loop
            if T.Nodes (Positive (N)).Children (I) /= Null_Node then
               -- Would need to track path for full implementation
               Collect (T.Nodes (Positive (N)).Children (I), Depth + 1);
            end if;
         end loop;
      end Collect;

   begin
      Suggestions := (others => (others => ' '));
      Lengths := (others => 0);
      Count := 0;

      if T.Root = Null_Node then
         return;
      end if;

      -- Navigate to prefix node
      for C of Prefix loop
         Idx := Char_Index (C);
         if Idx = 0 then
            return;
         end if;

         Current := T.Nodes (Positive (Current)).Children (Idx);
         if Current = Null_Node then
            return;
         end if;
      end loop;

      -- Check if prefix itself is a word
      if T.Nodes (Positive (Current)).Is_End_Word then
         Count := 1;
         for I in 1 .. Prefix'Length loop
            Suggestions (1) (I) := Prefix (Prefix'First + I - 1);
         end loop;
         Lengths (1) := Prefix'Length;
      end if;

      -- Collect from children (simplified - just returns prefix for now)
   end Autocomplete;

end GNAT.Trie;
