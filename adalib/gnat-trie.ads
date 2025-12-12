-- GNAT.Trie for Z80
-- Simple trie (prefix tree) implementation

package GNAT.Trie is
   pragma Preelaborate;

   Max_Nodes : constant := 64;   -- Maximum nodes for Z80
   Alphabet_Size : constant := 26;  -- a-z only for simplicity
   Max_Key_Length : constant := 16;

   type Trie is private;

   procedure Initialize (T : out Trie);
   --  Initialize empty trie

   procedure Insert (T : in Out Trie; Key : String; Success : out Boolean);
   --  Insert key into trie

   function Contains (T : Trie; Key : String) return Boolean;
   --  Check if key exists

   function Prefix_Count (T : Trie; Prefix : String) return Natural;
   --  Count keys with given prefix

   function Has_Prefix (T : Trie; Prefix : String) return Boolean;
   --  Check if any key starts with prefix

   procedure Remove (T : in Out Trie; Key : String);
   --  Remove key from trie

   function Size (T : Trie) return Natural;
   --  Return number of keys

   function Is_Empty (T : Trie) return Boolean;
   --  Check if trie is empty

   procedure Clear (T : out Trie);
   --  Clear all keys

   -- Autocomplete support
   Max_Suggestions : constant := 8;

   type Suggestion_Array is array (1 .. Max_Suggestions) of
     String (1 .. Max_Key_Length);
   type Suggestion_Lengths is array (1 .. Max_Suggestions) of Natural;

   procedure Autocomplete
     (T           : Trie;
      Prefix      : String;
      Suggestions : out Suggestion_Array;
      Lengths     : out Suggestion_Lengths;
      Count       : out Natural);
   --  Get up to Max_Suggestions completions for prefix

private

   type Node_Index is range 0 .. Max_Nodes;
   Null_Node : constant Node_Index := 0;

   type Children_Array is array (1 .. Alphabet_Size) of Node_Index;

   type Node is record
      Children    : Children_Array := (others => Null_Node);
      Is_End_Word : Boolean := False;
      Word_Count  : Natural := 0;  -- Words passing through this node
      In_Use      : Boolean := False;
   end record;

   type Node_Array is array (1 .. Max_Nodes) of Node;

   type Trie is record
      Nodes      : Node_Array;
      Root       : Node_Index := Null_Node;
      Node_Count : Natural := 0;
      Word_Count : Natural := 0;
   end record;

end GNAT.Trie;
