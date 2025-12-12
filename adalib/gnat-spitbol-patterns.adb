-- GNAT.Spitbol.Patterns body for Z80
-- SPITBOL-style pattern matching implementation

package body GNAT.Spitbol.Patterns is

   -- Helper: allocate a pattern node
   function Alloc_Node return Pattern_Node_Access is
   begin
      return new Pattern_Node;
   end Alloc_Node;

   ---------
   -- Any --
   ---------

   function Any (Str : String) return Pattern is
      N : constant Pattern_Node_Access := Alloc_Node;
   begin
      N.Kind := P_Any;
      N.Length := Natural'Min (Str'Length, 32);
      N.Chars (1 .. N.Length) := Str (Str'First .. Str'First + N.Length - 1);
      return (Root => N);
   end Any;

   -----------
   -- Break --
   -----------

   function Break (Str : String) return Pattern is
      N : constant Pattern_Node_Access := Alloc_Node;
   begin
      N.Kind := P_Break;
      N.Length := Natural'Min (Str'Length, 32);
      N.Chars (1 .. N.Length) := Str (Str'First .. Str'First + N.Length - 1);
      return (Root => N);
   end Break;

   ----------
   -- Span --
   ----------

   function Span (Str : String) return Pattern is
      N : constant Pattern_Node_Access := Alloc_Node;
   begin
      N.Kind := P_Span;
      N.Length := Natural'Min (Str'Length, 32);
      N.Chars (1 .. N.Length) := Str (Str'First .. Str'First + N.Length - 1);
      return (Root => N);
   end Span;

   ------------
   -- Notany --
   ------------

   function Notany (Str : String) return Pattern is
      N : constant Pattern_Node_Access := Alloc_Node;
   begin
      N.Kind := P_Notany;
      N.Length := Natural'Min (Str'Length, 32);
      N.Chars (1 .. N.Length) := Str (Str'First .. Str'First + N.Length - 1);
      return (Root => N);
   end Notany;

   ---------
   -- Len --
   ---------

   function Len (N : Natural) return Pattern is
      Node : constant Pattern_Node_Access := Alloc_Node;
   begin
      Node.Kind := P_Len;
      Node.Count := N;
      return (Root => Node);
   end Len;

   ---------
   -- Arb --
   ---------

   function Arb return Pattern is
      N : constant Pattern_Node_Access := Alloc_Node;
   begin
      N.Kind := P_Arb;
      return (Root => N);
   end Arb;

   -----------
   -- Arbno --
   -----------

   function Arbno (P : Pattern) return Pattern is
      N : constant Pattern_Node_Access := Alloc_Node;
   begin
      N.Kind := P_Arbno;
      N.Left := P.Root;
      return (Root => N);
   end Arbno;

   ----------
   -- Fail --
   ----------

   function Fail return Pattern is
      N : constant Pattern_Node_Access := Alloc_Node;
   begin
      N.Kind := P_Fail;
      return (Root => N);
   end Fail;

   -------------
   -- Succeed --
   -------------

   function Succeed return Pattern is
      N : constant Pattern_Node_Access := Alloc_Node;
   begin
      N.Kind := P_Succeed;
      return (Root => N);
   end Succeed;

   -----------
   -- Fence --
   -----------

   function Fence return Pattern is
      N : constant Pattern_Node_Access := Alloc_Node;
   begin
      N.Kind := P_Fence;
      return (Root => N);
   end Fence;

   ---------
   -- "&" --
   ---------

   function "&" (L, R : Pattern) return Pattern is
      N : constant Pattern_Node_Access := Alloc_Node;
   begin
      N.Kind := P_Concat;
      N.Left := L.Root;
      N.Right := R.Root;
      return (Root => N);
   end "&";

   -------------
   -- Or_Else --
   -------------

   function Or_Else (L, R : Pattern) return Pattern is
      N : constant Pattern_Node_Access := Alloc_Node;
   begin
      N.Kind := P_Alt;
      N.Left := L.Root;
      N.Right := R.Root;
      return (Root => N);
   end Or_Else;

   -- Helper: check if char is in set
   function In_Set (C : Character; Chars : String) return Boolean is
   begin
      for I in Chars'Range loop
         if Chars (I) = C then
            return True;
         end if;
      end loop;
      return False;
   end In_Set;

   -- Internal match function
   function Try_Match
     (Subject : String;
      Pos     : Natural;
      Node    : Pattern_Node_Access;
      Matched : out Natural) return Boolean
   is
   begin
      Matched := 0;

      if Node = null then
         return True;
      end if;

      case Node.Kind is
         when P_Null =>
            return True;

         when P_Any =>
            if Pos > Subject'Last then
               return False;
            end if;
            if In_Set (Subject (Pos), Node.Chars (1 .. Node.Length)) then
               Matched := 1;
               return True;
            end if;
            return False;

         when P_Break =>
            declare
               I : Natural := Pos;
            begin
               while I <= Subject'Last loop
                  if In_Set (Subject (I), Node.Chars (1 .. Node.Length)) then
                     Matched := I - Pos;
                     return True;
                  end if;
                  I := I + 1;
               end loop;
               Matched := Subject'Last - Pos + 1;
               return True;
            end;

         when P_Span =>
            declare
               I : Natural := Pos;
            begin
               while I <= Subject'Last
                 and then In_Set (Subject (I), Node.Chars (1 .. Node.Length))
               loop
                  I := I + 1;
               end loop;
               if I = Pos then
                  return False;
               end if;
               Matched := I - Pos;
               return True;
            end;

         when P_Notany =>
            if Pos > Subject'Last then
               return False;
            end if;
            if not In_Set (Subject (Pos), Node.Chars (1 .. Node.Length)) then
               Matched := 1;
               return True;
            end if;
            return False;

         when P_Len =>
            if Pos + Node.Count - 1 > Subject'Last then
               return False;
            end if;
            Matched := Node.Count;
            return True;

         when P_Arb =>
            -- Matches anything (greedy not implemented for simplicity)
            Matched := 0;
            return True;

         when P_Succeed =>
            return True;

         when P_Fail =>
            return False;

         when P_Fence =>
            return True;

         when P_Concat =>
            declare
               M1, M2 : Natural;
            begin
               if Try_Match (Subject, Pos, Node.Left, M1) then
                  if Try_Match (Subject, Pos + M1, Node.Right, M2) then
                     Matched := M1 + M2;
                     return True;
                  end if;
               end if;
               return False;
            end;

         when P_Alt =>
            declare
               M : Natural;
            begin
               if Try_Match (Subject, Pos, Node.Left, M) then
                  Matched := M;
                  return True;
               end if;
               if Try_Match (Subject, Pos, Node.Right, M) then
                  Matched := M;
                  return True;
               end if;
               return False;
            end;

         when P_Arbno =>
            Matched := 0;
            return True;  -- Simplified
      end case;
   end Try_Match;

   -----------
   -- Match --
   -----------

   function Match
     (Subject : String;
      Pat     : Pattern) return Boolean
   is
      Start_Pos, Stop_Pos : Natural;
   begin
      return Match (Subject, Pat, Start_Pos, Stop_Pos);
   end Match;

   function Match
     (Subject : String;
      Pat     : Pattern;
      Start   : out Natural;
      Stop    : out Natural) return Boolean
   is
      M : Natural;
   begin
      Start := 0;
      Stop := 0;

      -- Try matching at each position
      for I in Subject'Range loop
         if Try_Match (Subject, I, Pat.Root, M) then
            Start := I;
            Stop := I + M - 1;
            return True;
         end if;
      end loop;

      return False;
   end Match;

end GNAT.Spitbol.Patterns;
