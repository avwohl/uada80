-- GNAT.Statistical body for Z80
-- Basic statistical functions implementation

package body GNAT.Statistical is

   -- Simple integer square root
   function Sqrt_Int (X : Natural) return Natural is
      Guess : Natural;
      Prev  : Natural;
   begin
      if X = 0 then
         return 0;
      end if;

      Guess := X / 2;
      if Guess = 0 then
         Guess := 1;
      end if;

      for I in 1 .. 10 loop
         Prev := Guess;
         Guess := (Guess + X / Guess) / 2;
         exit when Guess = Prev;
      end loop;

      return Guess;
   end Sqrt_Int;

   ----------
   -- Sort --
   ----------

   procedure Sort (Data : in Out Data_Array) is
      Temp : Integer;
   begin
      -- Simple insertion sort for Z80
      for I in Data'First + 1 .. Data'Last loop
         Temp := Data (I);
         declare
            J : Integer := I - 1;
         begin
            while J >= Data'First and then Data (J) > Temp loop
               Data (J + 1) := Data (J);
               J := J - 1;
            end loop;
            Data (J + 1) := Temp;
         end;
      end loop;
   end Sort;

   ----------
   -- Mean --
   ----------

   function Mean (Data : Data_Array) return Integer is
   begin
      if Data'Length = 0 then
         return 0;
      end if;
      return Sum (Data) / Data'Length;
   end Mean;

   ------------
   -- Median --
   ------------

   function Median (Data : Data_Array) return Integer is
      Sorted : Data_Array (1 .. Data'Length);
      Mid    : constant Positive := (Data'Length + 1) / 2;
   begin
      if Data'Length = 0 then
         return 0;
      end if;

      -- Copy and sort
      for I in Data'Range loop
         Sorted (I - Data'First + 1) := Data (I);
      end loop;
      Sort (Sorted);

      if Data'Length mod 2 = 0 then
         return (Sorted (Mid) + Sorted (Mid + 1)) / 2;
      else
         return Sorted (Mid);
      end if;
   end Median;

   ----------
   -- Mode --
   ----------

   function Mode (Data : Data_Array) return Integer is
      Max_Count   : Natural := 0;
      Mode_Value  : Integer := 0;
      Curr_Count  : Natural;
   begin
      if Data'Length = 0 then
         return 0;
      end if;

      for I in Data'Range loop
         Curr_Count := Count (Data, Data (I));
         if Curr_Count > Max_Count then
            Max_Count := Curr_Count;
            Mode_Value := Data (I);
         end if;
      end loop;

      return Mode_Value;
   end Mode;

   ---------
   -- Min --
   ---------

   function Min (Data : Data_Array) return Integer is
      Result : Integer := Integer'Last;
   begin
      for V of Data loop
         if V < Result then
            Result := V;
         end if;
      end loop;
      return Result;
   end Min;

   ---------
   -- Max --
   ---------

   function Max (Data : Data_Array) return Integer is
      Result : Integer := Integer'First;
   begin
      for V of Data loop
         if V > Result then
            Result := V;
         end if;
      end loop;
      return Result;
   end Max;

   ---------------
   -- Range_Val --
   ---------------

   function Range_Val (Data : Data_Array) return Integer is
   begin
      return Max (Data) - Min (Data);
   end Range_Val;

   --------------
   -- Variance --
   --------------

   function Variance (Data : Data_Array) return Integer is
      M   : constant Integer := Mean (Data);
      Sum : Integer := 0;
      D   : Integer;
   begin
      if Data'Length = 0 then
         return 0;
      end if;

      for V of Data loop
         D := V - M;
         Sum := Sum + D * D;
      end loop;

      return (Sum * 100) / Data'Length;
   end Variance;

   -------------------
   -- Std_Deviation --
   -------------------

   function Std_Deviation (Data : Data_Array) return Integer is
      V : constant Integer := Variance (Data);
   begin
      -- Variance is scaled by 100, so sqrt gives us * 10
      return Sqrt_Int (Natural (abs V));
   end Std_Deviation;

   ---------
   -- Sum --
   ---------

   function Sum (Data : Data_Array) return Integer is
      Result : Integer := 0;
   begin
      for V of Data loop
         Result := Result + V;
      end loop;
      return Result;
   end Sum;

   --------------------
   -- Sum_Of_Squares --
   --------------------

   function Sum_Of_Squares (Data : Data_Array) return Integer is
      Result : Integer := 0;
   begin
      for V of Data loop
         Result := Result + V * V;
      end loop;
      return Result;
   end Sum_Of_Squares;

   -------------
   -- Product --
   -------------

   function Product (Data : Data_Array) return Integer is
      Result : Integer := 1;
   begin
      for V of Data loop
         Result := Result * V;
      end loop;
      return Result;
   end Product;

   -----------
   -- Count --
   -----------

   function Count (Data : Data_Array; Value : Integer) return Natural is
      Result : Natural := 0;
   begin
      for V of Data loop
         if V = Value then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count;

   --------------------
   -- Count_In_Range --
   --------------------

   function Count_In_Range (Data : Data_Array; Low, High : Integer) return Natural is
      Result : Natural := 0;
   begin
      for V of Data loop
         if V >= Low and V <= High then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count_In_Range;

   -----------------
   -- Count_Above --
   -----------------

   function Count_Above (Data : Data_Array; Threshold : Integer) return Natural is
      Result : Natural := 0;
   begin
      for V of Data loop
         if V > Threshold then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count_Above;

   -----------------
   -- Count_Below --
   -----------------

   function Count_Below (Data : Data_Array; Threshold : Integer) return Natural is
      Result : Natural := 0;
   begin
      for V of Data loop
         if V < Threshold then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count_Below;

   ----------------
   -- Percentile --
   ----------------

   function Percentile (Data : Data_Array; P : Natural) return Integer is
      Sorted : Data_Array (1 .. Data'Length);
      Idx    : Positive;
   begin
      if Data'Length = 0 or P > 100 then
         return 0;
      end if;

      for I in Data'Range loop
         Sorted (I - Data'First + 1) := Data (I);
      end loop;
      Sort (Sorted);

      Idx := (Data'Length * P) / 100;
      if Idx < 1 then
         Idx := 1;
      end if;
      if Idx > Data'Length then
         Idx := Data'Length;
      end if;

      return Sorted (Idx);
   end Percentile;

   ----------------
   -- Quartile_1 --
   ----------------

   function Quartile_1 (Data : Data_Array) return Integer is
   begin
      return Percentile (Data, 25);
   end Quartile_1;

   ----------------
   -- Quartile_3 --
   ----------------

   function Quartile_3 (Data : Data_Array) return Integer is
   begin
      return Percentile (Data, 75);
   end Quartile_3;

   ---------
   -- IQR --
   ---------

   function IQR (Data : Data_Array) return Integer is
   begin
      return Quartile_3 (Data) - Quartile_1 (Data);
   end IQR;

   -----------------
   -- Correlation --
   -----------------

   function Correlation (X, Y : Data_Array) return Integer is
      N        : constant Natural := Natural'Min (X'Length, Y'Length);
      Sum_X    : Integer := 0;
      Sum_Y    : Integer := 0;
      Sum_XY   : Integer := 0;
      Sum_X2   : Integer := 0;
      Sum_Y2   : Integer := 0;
      Num, Den : Integer;
   begin
      if N < 2 then
         return 0;
      end if;

      for I in 1 .. N loop
         Sum_X := Sum_X + X (X'First + I - 1);
         Sum_Y := Sum_Y + Y (Y'First + I - 1);
         Sum_XY := Sum_XY + X (X'First + I - 1) * Y (Y'First + I - 1);
         Sum_X2 := Sum_X2 + X (X'First + I - 1) * X (X'First + I - 1);
         Sum_Y2 := Sum_Y2 + Y (Y'First + I - 1) * Y (Y'First + I - 1);
      end loop;

      Num := N * Sum_XY - Sum_X * Sum_Y;
      Den := Sqrt_Int (Natural (abs (N * Sum_X2 - Sum_X * Sum_X))) *
             Sqrt_Int (Natural (abs (N * Sum_Y2 - Sum_Y * Sum_Y)));

      if Den = 0 then
         return 0;
      end if;

      return (Num * 100) / Den;
   end Correlation;

   -----------------------
   -- Linear_Regression --
   -----------------------

   procedure Linear_Regression
     (X, Y      : Data_Array;
      Slope     : out Integer;
      Intercept : out Integer)
   is
      N       : constant Natural := Natural'Min (X'Length, Y'Length);
      Sum_X   : Integer := 0;
      Sum_Y   : Integer := 0;
      Sum_XY  : Integer := 0;
      Sum_X2  : Integer := 0;
      Denom   : Integer;
   begin
      Slope := 0;
      Intercept := 0;

      if N < 2 then
         return;
      end if;

      for I in 1 .. N loop
         Sum_X := Sum_X + X (X'First + I - 1);
         Sum_Y := Sum_Y + Y (Y'First + I - 1);
         Sum_XY := Sum_XY + X (X'First + I - 1) * Y (Y'First + I - 1);
         Sum_X2 := Sum_X2 + X (X'First + I - 1) * X (X'First + I - 1);
      end loop;

      Denom := N * Sum_X2 - Sum_X * Sum_X;
      if Denom = 0 then
         return;
      end if;

      Slope := ((N * Sum_XY - Sum_X * Sum_Y) * 100) / Denom;
      Intercept := (Sum_Y - (Slope * Sum_X) / 100) / N;
   end Linear_Regression;

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize
     (Data   : Data_Array;
      Result : out Data_Array;
      Scale  : Positive := 100)
   is
      Min_Val : constant Integer := Min (Data);
      Max_Val : constant Integer := Max (Data);
      R       : Integer;
   begin
      R := Max_Val - Min_Val;
      if R = 0 then
         R := 1;
      end if;

      for I in Data'Range loop
         if I - Data'First + 1 <= Result'Length then
            Result (Result'First + I - Data'First) :=
              ((Data (I) - Min_Val) * Scale) / R;
         end if;
      end loop;
   end Normalize;

   -------------
   -- Z_Score --
   -------------

   function Z_Score (Data : Data_Array; Value : Integer) return Integer is
      M  : constant Integer := Mean (Data);
      SD : constant Integer := Std_Deviation (Data);
   begin
      if SD = 0 then
         return 0;
      end if;
      return ((Value - M) * 10) / (SD / 10);
   end Z_Score;

end GNAT.Statistical;
