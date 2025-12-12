-- GNAT.Sort_Utils body for Z80
-- Sorting utilities implementation

package body GNAT.Sort_Utils is

   ----------
   -- Swap --
   ----------

   procedure Swap (Data : in Out Int_Array; I, J : Positive) is
      Temp : constant Integer := Data (I);
   begin
      Data (I) := Data (J);
      Data (J) := Temp;
   end Swap;

   -------------------
   -- Sort_Integers --
   -------------------

   procedure Sort_Integers (Data : in Out Int_Array) is
      Temp : Integer;
      J    : Integer;
   begin
      -- Insertion sort: efficient for small arrays on Z80
      for I in Data'First + 1 .. Data'Last loop
         Temp := Data (I);
         J := I - 1;
         while J >= Data'First and then Data (J) > Temp loop
            Data (J + 1) := Data (J);
            J := J - 1;
         end loop;
         Data (J + 1) := Temp;
      end loop;
   end Sort_Integers;

   ------------------------
   -- Sort_Integers_Desc --
   ------------------------

   procedure Sort_Integers_Desc (Data : in Out Int_Array) is
      Temp : Integer;
      J    : Integer;
   begin
      for I in Data'First + 1 .. Data'Last loop
         Temp := Data (I);
         J := I - 1;
         while J >= Data'First and then Data (J) < Temp loop
            Data (J + 1) := Data (J);
            J := J - 1;
         end loop;
         Data (J + 1) := Temp;
      end loop;
   end Sort_Integers_Desc;

   ---------------
   -- Is_Sorted --
   ---------------

   function Is_Sorted (Data : Int_Array) return Boolean is
   begin
      for I in Data'First .. Data'Last - 1 loop
         if Data (I) > Data (I + 1) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Sorted;

   --------------------
   -- Is_Sorted_Desc --
   --------------------

   function Is_Sorted_Desc (Data : Int_Array) return Boolean is
   begin
      for I in Data'First .. Data'Last - 1 loop
         if Data (I) < Data (I + 1) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Sorted_Desc;

   ---------------
   -- Min_Index --
   ---------------

   function Min_Index (Data : Int_Array) return Positive is
      Min_Idx : Positive := Data'First;
   begin
      for I in Data'First + 1 .. Data'Last loop
         if Data (I) < Data (Min_Idx) then
            Min_Idx := I;
         end if;
      end loop;
      return Min_Idx;
   end Min_Index;

   ---------------
   -- Max_Index --
   ---------------

   function Max_Index (Data : Int_Array) return Positive is
      Max_Idx : Positive := Data'First;
   begin
      for I in Data'First + 1 .. Data'Last loop
         if Data (I) > Data (Max_Idx) then
            Max_Idx := I;
         end if;
      end loop;
      return Max_Idx;
   end Max_Index;

   -------------------
   -- Binary_Search --
   -------------------

   function Binary_Search (Data : Int_Array; Value : Integer) return Natural is
      Low  : Integer := Data'First;
      High : Integer := Data'Last;
      Mid  : Integer;
   begin
      while Low <= High loop
         Mid := (Low + High) / 2;
         if Data (Mid) = Value then
            return Mid;
         elsif Data (Mid) < Value then
            Low := Mid + 1;
         else
            High := Mid - 1;
         end if;
      end loop;
      return 0;
   end Binary_Search;

   -------------------
   -- Linear_Search --
   -------------------

   function Linear_Search (Data : Int_Array; Value : Integer) return Natural is
   begin
      for I in Data'Range loop
         if Data (I) = Value then
            return I;
         end if;
      end loop;
      return 0;
   end Linear_Search;

   -------------------
   -- Reverse_Array --
   -------------------

   procedure Reverse_Array (Data : in Out Int_Array) is
      Left  : Integer := Data'First;
      Right : Integer := Data'Last;
   begin
      while Left < Right loop
         Swap (Data, Left, Right);
         Left := Left + 1;
         Right := Right - 1;
      end loop;
   end Reverse_Array;

   -----------------
   -- Rotate_Left --
   -----------------

   procedure Rotate_Left (Data : in Out Int_Array; N : Natural) is
      Actual_N : constant Natural := N mod Data'Length;
      Temp     : Int_Array (1 .. Actual_N);
   begin
      if Actual_N = 0 or Data'Length <= 1 then
         return;
      end if;

      -- Save first N elements
      for I in 1 .. Actual_N loop
         Temp (I) := Data (Data'First + I - 1);
      end loop;

      -- Shift remaining elements left
      for I in Data'First .. Data'Last - Actual_N loop
         Data (I) := Data (I + Actual_N);
      end loop;

      -- Place saved elements at end
      for I in 1 .. Actual_N loop
         Data (Data'Last - Actual_N + I) := Temp (I);
      end loop;
   end Rotate_Left;

   ------------------
   -- Rotate_Right --
   ------------------

   procedure Rotate_Right (Data : in Out Int_Array; N : Natural) is
   begin
      if Data'Length > 0 then
         Rotate_Left (Data, Data'Length - (N mod Data'Length));
      end if;
   end Rotate_Right;

   -------------
   -- Shuffle --
   -------------

   procedure Shuffle (Data : in Out Int_Array; Seed : in Out Natural) is
      J : Natural;
   begin
      for I in reverse Data'First + 1 .. Data'Last loop
         -- Simple LCG random
         Seed := (Seed * 1103515245 + 12345) mod 65536;
         J := Data'First + (Seed mod (I - Data'First + 1));
         if J /= I then
            Swap (Data, I, J);
         end if;
      end loop;
   end Shuffle;

   ---------------
   -- Partition --
   ---------------

   function Partition (Data : in Out Int_Array;
                       Low, High : Positive) return Positive is
      Pivot : constant Integer := Data (High);
      I     : Integer := Low - 1;
   begin
      for J in Low .. High - 1 loop
         if Data (J) <= Pivot then
            I := I + 1;
            Swap (Data, I, J);
         end if;
      end loop;
      Swap (Data, I + 1, High);
      return I + 1;
   end Partition;

   ------------------
   -- Kth_Smallest --
   ------------------

   function Kth_Smallest (Data : in Out Int_Array;
                          K : Positive) return Integer is
      Low  : Integer := Data'First;
      High : Integer := Data'Last;
      Pivot_Idx : Integer;
   begin
      while Low <= High loop
         Pivot_Idx := Partition (Data, Low, High);
         if Pivot_Idx = Data'First + K - 1 then
            return Data (Pivot_Idx);
         elsif Pivot_Idx > Data'First + K - 1 then
            High := Pivot_Idx - 1;
         else
            Low := Pivot_Idx + 1;
         end if;
      end loop;
      return Data (Low);
   end Kth_Smallest;

   -----------------------
   -- Remove_Duplicates --
   -----------------------

   function Remove_Duplicates (Data : in Out Int_Array) return Natural is
      Write_Idx : Integer;
   begin
      if Data'Length <= 1 then
         return Data'Length;
      end if;

      Write_Idx := Data'First;
      for Read_Idx in Data'First + 1 .. Data'Last loop
         if Data (Read_Idx) /= Data (Write_Idx) then
            Write_Idx := Write_Idx + 1;
            Data (Write_Idx) := Data (Read_Idx);
         end if;
      end loop;

      return Write_Idx - Data'First + 1;
   end Remove_Duplicates;

   ------------------
   -- Merge_Sorted --
   ------------------

   procedure Merge_Sorted (A, B : Int_Array;
                           Result : out Int_Array;
                           Length : out Natural) is
      AI : Integer := A'First;
      BI : Integer := B'First;
      RI : Integer := Result'First;
   begin
      Length := 0;

      while AI <= A'Last and BI <= B'Last and RI <= Result'Last loop
         if A (AI) <= B (BI) then
            Result (RI) := A (AI);
            AI := AI + 1;
         else
            Result (RI) := B (BI);
            BI := BI + 1;
         end if;
         RI := RI + 1;
         Length := Length + 1;
      end loop;

      while AI <= A'Last and RI <= Result'Last loop
         Result (RI) := A (AI);
         AI := AI + 1;
         RI := RI + 1;
         Length := Length + 1;
      end loop;

      while BI <= B'Last and RI <= Result'Last loop
         Result (RI) := B (BI);
         BI := BI + 1;
         RI := RI + 1;
         Length := Length + 1;
      end loop;
   end Merge_Sorted;

   ----------
   -- Copy --
   ----------

   procedure Copy (Source : Int_Array;
                   Dest : out Int_Array;
                   Count : Natural) is
      C : Natural := Count;
   begin
      if C > Source'Length then
         C := Source'Length;
      end if;
      if C > Dest'Length then
         C := Dest'Length;
      end if;

      for I in 1 .. C loop
         Dest (Dest'First + I - 1) := Source (Source'First + I - 1);
      end loop;
   end Copy;

   ----------
   -- Fill --
   ----------

   procedure Fill (Data : out Int_Array; Value : Integer) is
   begin
      for I in Data'Range loop
         Data (I) := Value;
      end loop;
   end Fill;

   -------------------
   -- Fill_Sequence --
   -------------------

   procedure Fill_Sequence (Data : out Int_Array;
                            Start : Integer := 1;
                            Step : Integer := 1) is
      V : Integer := Start;
   begin
      for I in Data'Range loop
         Data (I) := V;
         V := V + Step;
      end loop;
   end Fill_Sequence;

   ---------------------
   -- To_Fixed_String --
   ---------------------

   function To_Fixed_String (S : String) return Fixed_String is
      Result : Fixed_String;
   begin
      Result.Data := (others => ' ');
      Result.Len := S'Length;
      if Result.Len > Max_Str_Len then
         Result.Len := Max_Str_Len;
      end if;
      for I in 1 .. Result.Len loop
         Result.Data (I) := S (S'First + I - 1);
      end loop;
      return Result;
   end To_Fixed_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (FS : Fixed_String) return String is
   begin
      return FS.Data (1 .. FS.Len);
   end To_String;

   -----------------
   -- String_Less --
   -----------------

   function String_Less (A, B : Fixed_String) return Boolean is
      Min_Len : constant Natural := Natural'Min (A.Len, B.Len);
   begin
      for I in 1 .. Min_Len loop
         if A.Data (I) < B.Data (I) then
            return True;
         elsif A.Data (I) > B.Data (I) then
            return False;
         end if;
      end loop;
      return A.Len < B.Len;
   end String_Less;

   --------------------
   -- String_Less_CI --
   --------------------

   function String_Less_CI (A, B : Fixed_String) return Boolean is
      Min_Len : constant Natural := Natural'Min (A.Len, B.Len);

      function To_Upper (C : Character) return Character is
      begin
         if C >= 'a' and C <= 'z' then
            return Character'Val (Character'Pos (C) - 32);
         else
            return C;
         end if;
      end To_Upper;

   begin
      for I in 1 .. Min_Len loop
         declare
            CA : constant Character := To_Upper (A.Data (I));
            CB : constant Character := To_Upper (B.Data (I));
         begin
            if CA < CB then
               return True;
            elsif CA > CB then
               return False;
            end if;
         end;
      end loop;
      return A.Len < B.Len;
   end String_Less_CI;

   ------------------
   -- Sort_Strings --
   ------------------

   procedure Sort_Strings (Data : in Out Str_Array) is
      Temp : Fixed_String;
      J    : Integer;
   begin
      for I in Data'First + 1 .. Data'Last loop
         Temp := Data (I);
         J := I - 1;
         while J >= Data'First and then String_Less (Temp, Data (J)) loop
            Data (J + 1) := Data (J);
            J := J - 1;
         end loop;
         Data (J + 1) := Temp;
      end loop;
   end Sort_Strings;

   ---------------------
   -- Sort_Strings_CI --
   ---------------------

   procedure Sort_Strings_CI (Data : in Out Str_Array) is
      Temp : Fixed_String;
      J    : Integer;
   begin
      for I in Data'First + 1 .. Data'Last loop
         Temp := Data (I);
         J := I - 1;
         while J >= Data'First and then String_Less_CI (Temp, Data (J)) loop
            Data (J + 1) := Data (J);
            J := J - 1;
         end loop;
         Data (J + 1) := Temp;
      end loop;
   end Sort_Strings_CI;

end GNAT.Sort_Utils;
