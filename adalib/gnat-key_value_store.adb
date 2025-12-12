-- GNAT.Key_Value_Store body for Z80
-- Key-value storage implementation

package body GNAT.Key_Value_Store is

   function Keys_Equal (K1 : String; K1_Len : Natural;
                        K2 : String) return Boolean is
   begin
      if K1_Len /= K2'Length then
         return False;
      end if;
      for I in 1 .. K1_Len loop
         if K1 (I) /= K2 (K2'First + I - 1) then
            return False;
         end if;
      end loop;
      return True;
   end Keys_Equal;

   function Find_Entry (S : KV_Store; Key : String) return Natural is
   begin
      for I in 1 .. Max_Entries loop
         if S.Entries (I).Used and then
            Keys_Equal (S.Entries (I).Key, S.Entries (I).Key_Len, Key) then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Entry;

   function Find_Free (S : KV_Store) return Natural is
   begin
      for I in 1 .. Max_Entries loop
         if not S.Entries (I).Used then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Free;

   function Starts_With (S : String; S_Len : Natural;
                         Prefix : String) return Boolean is
   begin
      if Prefix'Length > S_Len then
         return False;
      end if;
      for I in 1 .. Prefix'Length loop
         if S (I) /= Prefix (Prefix'First + I - 1) then
            return False;
         end if;
      end loop;
      return True;
   end Starts_With;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (S : out KV_Store) is
   begin
      S.Count := 0;
      for I in 1 .. Max_Entries loop
         S.Entries (I).Used := False;
         S.Entries (I).Key_Len := 0;
         S.Entries (I).Val_Len := 0;
      end loop;
   end Initialize;

   ---------
   -- Put --
   ---------

   procedure Put (S : in Out KV_Store; Key, Value : String) is
      Idx : Natural;
      KL  : Natural;
      VL  : Natural;
   begin
      Idx := Find_Entry (S, Key);

      if Idx = 0 then
         Idx := Find_Free (S);
         if Idx = 0 then
            return;  -- Store is full
         end if;
         S.Count := S.Count + 1;
      end if;

      -- Store key
      S.Entries (Idx).Key := (others => ' ');
      KL := Key'Length;
      if KL > Max_Key_Length then
         KL := Max_Key_Length;
      end if;
      S.Entries (Idx).Key_Len := KL;
      for I in 1 .. KL loop
         S.Entries (Idx).Key (I) := Key (Key'First + I - 1);
      end loop;

      -- Store value
      S.Entries (Idx).Value := (others => ' ');
      VL := Value'Length;
      if VL > Max_Val_Length then
         VL := Max_Val_Length;
      end if;
      S.Entries (Idx).Val_Len := VL;
      for I in 1 .. VL loop
         S.Entries (Idx).Value (I) := Value (Value'First + I - 1);
      end loop;

      S.Entries (Idx).Used := True;
   end Put;

   ---------
   -- Get --
   ---------

   function Get (S : KV_Store; Key : String) return String is
      Idx : constant Natural := Find_Entry (S, Key);
   begin
      if Idx = 0 then
         return "";
      end if;
      return S.Entries (Idx).Value (1 .. S.Entries (Idx).Val_Len);
   end Get;

   --------------------
   -- Get_Or_Default --
   --------------------

   function Get_Or_Default (S : KV_Store; Key : String;
                            Default : String) return String is
      Idx : constant Natural := Find_Entry (S, Key);
   begin
      if Idx = 0 then
         return Default;
      end if;
      return S.Entries (Idx).Value (1 .. S.Entries (Idx).Val_Len);
   end Get_Or_Default;

   --------------
   -- Contains --
   --------------

   function Contains (S : KV_Store; Key : String) return Boolean is
   begin
      return Find_Entry (S, Key) /= 0;
   end Contains;

   ------------
   -- Delete --
   ------------

   procedure Delete (S : in Out KV_Store; Key : String) is
      Idx : constant Natural := Find_Entry (S, Key);
   begin
      if Idx /= 0 then
         S.Entries (Idx).Used := False;
         S.Count := S.Count - 1;
      end if;
   end Delete;

   -------------
   -- Put_Int --
   -------------

   procedure Put_Int (S : in Out KV_Store; Key : String; Value : Integer) is
      Buf : String (1 .. 12);
      Pos : Natural := 12;
      V   : Integer := abs Value;
   begin
      if V = 0 then
         Put (S, Key, "0");
         return;
      end if;

      while V > 0 loop
         Buf (Pos) := Character'Val (Character'Pos ('0') + (V mod 10));
         V := V / 10;
         Pos := Pos - 1;
      end loop;

      if Value < 0 then
         Buf (Pos) := '-';
         Pos := Pos - 1;
      end if;

      Put (S, Key, Buf (Pos + 1 .. 12));
   end Put_Int;

   -------------
   -- Get_Int --
   -------------

   function Get_Int (S : KV_Store; Key : String) return Integer is
      Val : constant String := Get (S, Key);
      Result : Integer := 0;
      Negative : Boolean := False;
      Start : Natural := Val'First;
   begin
      if Val'Length = 0 then
         return 0;
      end if;

      if Val (Start) = '-' then
         Negative := True;
         Start := Start + 1;
      elsif Val (Start) = '+' then
         Start := Start + 1;
      end if;

      for I in Start .. Val'Last loop
         exit when Val (I) < '0' or Val (I) > '9';
         Result := Result * 10 + (Character'Pos (Val (I)) - Character'Pos ('0'));
      end loop;

      if Negative then
         return -Result;
      else
         return Result;
      end if;
   end Get_Int;

   ------------------------
   -- Get_Int_Or_Default --
   ------------------------

   function Get_Int_Or_Default (S : KV_Store; Key : String;
                                Default : Integer) return Integer is
   begin
      if Contains (S, Key) then
         return Get_Int (S, Key);
      else
         return Default;
      end if;
   end Get_Int_Or_Default;

   --------------
   -- Put_Bool --
   --------------

   procedure Put_Bool (S : in Out KV_Store; Key : String; Value : Boolean) is
   begin
      if Value then
         Put (S, Key, "1");
      else
         Put (S, Key, "0");
      end if;
   end Put_Bool;

   --------------
   -- Get_Bool --
   --------------

   function Get_Bool (S : KV_Store; Key : String) return Boolean is
      Val : constant String := Get (S, Key);
   begin
      if Val'Length > 0 then
         return Val (Val'First) = '1' or
                Val (Val'First) = 'T' or Val (Val'First) = 't' or
                Val (Val'First) = 'Y' or Val (Val'First) = 'y';
      else
         return False;
      end if;
   end Get_Bool;

   ---------------
   -- Increment --
   ---------------

   procedure Increment (S : in Out KV_Store; Key : String; Amount : Integer := 1) is
      Val : Integer;
   begin
      Val := Get_Int_Or_Default (S, Key, 0);
      Put_Int (S, Key, Val + Amount);
   end Increment;

   ---------------
   -- Decrement --
   ---------------

   procedure Decrement (S : in Out KV_Store; Key : String; Amount : Integer := 1) is
      Val : Integer;
   begin
      Val := Get_Int_Or_Default (S, Key, 0);
      Put_Int (S, Key, Val - Amount);
   end Decrement;

   -----------
   -- Clear --
   -----------

   procedure Clear (S : out KV_Store) is
   begin
      Initialize (S);
   end Clear;

   -----------
   -- Count --
   -----------

   function Count (S : KV_Store) return Natural is
   begin
      return S.Count;
   end Count;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (S : KV_Store) return Boolean is
   begin
      return S.Count = 0;
   end Is_Empty;

   -------------
   -- Is_Full --
   -------------

   function Is_Full (S : KV_Store) return Boolean is
   begin
      return S.Count = Max_Entries;
   end Is_Full;

   -----------
   -- First --
   -----------

   function First (S : KV_Store) return Cursor is
      C : Cursor;
   begin
      C.Index := 0;
      for I in 1 .. Max_Entries loop
         if S.Entries (I).Used then
            C.Index := I;
            exit;
         end if;
      end loop;
      return C;
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (C : Cursor) return Boolean is
   begin
      return C.Index > 0;
   end Has_Element;

   ----------
   -- Next --
   ----------

   procedure Next (S : KV_Store; C : in Out Cursor) is
   begin
      if C.Index = 0 then
         return;
      end if;

      for I in C.Index + 1 .. Max_Entries loop
         if S.Entries (I).Used then
            C.Index := I;
            return;
         end if;
      end loop;

      C.Index := 0;
   end Next;

   ---------
   -- Key --
   ---------

   function Key (S : KV_Store; C : Cursor) return String is
   begin
      if C.Index = 0 or else not S.Entries (C.Index).Used then
         return "";
      end if;
      return S.Entries (C.Index).Key (1 .. S.Entries (C.Index).Key_Len);
   end Key;

   -----------
   -- Value --
   -----------

   function Value (S : KV_Store; C : Cursor) return String is
   begin
      if C.Index = 0 or else not S.Entries (C.Index).Used then
         return "";
      end if;
      return S.Entries (C.Index).Value (1 .. S.Entries (C.Index).Val_Len);
   end Value;

   ------------
   -- Key_At --
   ------------

   function Key_At (S : KV_Store; Index : Positive) return String is
      Count : Natural := 0;
   begin
      for I in 1 .. Max_Entries loop
         if S.Entries (I).Used then
            Count := Count + 1;
            if Count = Index then
               return S.Entries (I).Key (1 .. S.Entries (I).Key_Len);
            end if;
         end if;
      end loop;
      return "";
   end Key_At;

   --------------
   -- Value_At --
   --------------

   function Value_At (S : KV_Store; Index : Positive) return String is
      Count : Natural := 0;
   begin
      for I in 1 .. Max_Entries loop
         if S.Entries (I).Used then
            Count := Count + 1;
            if Count = Index then
               return S.Entries (I).Value (1 .. S.Entries (I).Val_Len);
            end if;
         end if;
      end loop;
      return "";
   end Value_At;

   --------------------
   -- Find_By_Prefix --
   --------------------

   function Find_By_Prefix (S : KV_Store; Prefix : String) return String is
   begin
      for I in 1 .. Max_Entries loop
         if S.Entries (I).Used and then
            Starts_With (S.Entries (I).Key, S.Entries (I).Key_Len, Prefix) then
            return S.Entries (I).Key (1 .. S.Entries (I).Key_Len);
         end if;
      end loop;
      return "";
   end Find_By_Prefix;

   -----------------------
   -- Count_With_Prefix --
   -----------------------

   function Count_With_Prefix (S : KV_Store; Prefix : String) return Natural is
      Result : Natural := 0;
   begin
      for I in 1 .. Max_Entries loop
         if S.Entries (I).Used and then
            Starts_With (S.Entries (I).Key, S.Entries (I).Key_Len, Prefix) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count_With_Prefix;

   ------------------------
   -- Delete_With_Prefix --
   ------------------------

   procedure Delete_With_Prefix (S : in Out KV_Store; Prefix : String) is
   begin
      for I in 1 .. Max_Entries loop
         if S.Entries (I).Used and then
            Starts_With (S.Entries (I).Key, S.Entries (I).Key_Len, Prefix) then
            S.Entries (I).Used := False;
            S.Count := S.Count - 1;
         end if;
      end loop;
   end Delete_With_Prefix;

   ------------
   -- Rename --
   ------------

   procedure Rename (S : in Out KV_Store; Old_Key, New_Key : String) is
      Idx : constant Natural := Find_Entry (S, Old_Key);
      KL  : Natural;
   begin
      if Idx = 0 then
         return;
      end if;

      -- Check if new key already exists
      if Find_Entry (S, New_Key) /= 0 then
         return;
      end if;

      -- Update key
      S.Entries (Idx).Key := (others => ' ');
      KL := New_Key'Length;
      if KL > Max_Key_Length then
         KL := Max_Key_Length;
      end if;
      S.Entries (Idx).Key_Len := KL;
      for I in 1 .. KL loop
         S.Entries (Idx).Key (I) := New_Key (New_Key'First + I - 1);
      end loop;
   end Rename;

   ----------------
   -- Copy_Value --
   ----------------

   procedure Copy_Value (S : in Out KV_Store; From_Key, To_Key : String) is
      Val : constant String := Get (S, From_Key);
   begin
      if Val'Length > 0 then
         Put (S, To_Key, Val);
      end if;
   end Copy_Value;

   ------------
   -- Append --
   ------------

   procedure Append (S : in Out KV_Store; Key : String;
                     Value : String; Sep : String := "") is
      Idx : constant Natural := Find_Entry (S, Key);
      OldLen : Natural;
      NewLen : Natural;
      SepLen : Natural;
   begin
      if Idx = 0 then
         Put (S, Key, Value);
         return;
      end if;

      OldLen := S.Entries (Idx).Val_Len;
      SepLen := Sep'Length;
      NewLen := OldLen + SepLen + Value'Length;

      if NewLen > Max_Val_Length then
         NewLen := Max_Val_Length;
      end if;

      -- Add separator
      for I in 1 .. SepLen loop
         if OldLen + I <= Max_Val_Length then
            S.Entries (Idx).Value (OldLen + I) := Sep (Sep'First + I - 1);
         end if;
      end loop;

      -- Add value
      for I in 1 .. Value'Length loop
         if OldLen + SepLen + I <= Max_Val_Length then
            S.Entries (Idx).Value (OldLen + SepLen + I) :=
              Value (Value'First + I - 1);
         end if;
      end loop;

      S.Entries (Idx).Val_Len := NewLen;
   end Append;

   ------------------
   -- Value_Equals --
   ------------------

   function Value_Equals (S : KV_Store; Key : String;
                          Expected : String) return Boolean is
      Val : constant String := Get (S, Key);
   begin
      return Val = Expected;
   end Value_Equals;

end GNAT.Key_Value_Store;
