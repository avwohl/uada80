-- GNAT.Statistical for Z80
-- Basic statistical functions using integer arithmetic

package GNAT.Statistical is
   pragma Pure;

   Max_Data_Size : constant := 64;  -- Maximum data points for Z80

   type Data_Array is array (Positive range <>) of Integer;

   -- Central tendency
   function Mean (Data : Data_Array) return Integer;
   --  Arithmetic mean (truncated to integer)

   function Median (Data : Data_Array) return Integer;
   --  Median value (modifies internal copy for sorting)

   function Mode (Data : Data_Array) return Integer;
   --  Most frequent value (first if tie)

   -- Dispersion
   function Min (Data : Data_Array) return Integer;
   function Max (Data : Data_Array) return Integer;
   function Range_Val (Data : Data_Array) return Integer;
   --  Max - Min

   function Variance (Data : Data_Array) return Integer;
   --  Population variance (scaled by 100 for precision)

   function Std_Deviation (Data : Data_Array) return Integer;
   --  Standard deviation (scaled by 10 for precision)

   -- Sums
   function Sum (Data : Data_Array) return Integer;
   function Sum_Of_Squares (Data : Data_Array) return Integer;
   function Product (Data : Data_Array) return Integer;

   -- Counting
   function Count (Data : Data_Array; Value : Integer) return Natural;
   --  Count occurrences of value

   function Count_In_Range (Data : Data_Array; Low, High : Integer) return Natural;
   --  Count values in range [Low, High]

   function Count_Above (Data : Data_Array; Threshold : Integer) return Natural;
   function Count_Below (Data : Data_Array; Threshold : Integer) return Natural;

   -- Percentiles (approximate for small datasets)
   function Percentile (Data : Data_Array; P : Natural) return Integer;
   --  P is 0-100

   function Quartile_1 (Data : Data_Array) return Integer;
   function Quartile_3 (Data : Data_Array) return Integer;
   function IQR (Data : Data_Array) return Integer;  -- Interquartile range

   -- Correlation (scaled by 100)
   function Correlation (X, Y : Data_Array) return Integer;
   --  Pearson correlation coefficient * 100

   -- Linear regression (returns slope * 100 and intercept)
   procedure Linear_Regression
     (X, Y      : Data_Array;
      Slope     : out Integer;   -- Scaled by 100
      Intercept : out Integer);

   -- Normalization (to 0-100 range)
   procedure Normalize
     (Data   : Data_Array;
      Result : out Data_Array;
      Scale  : Positive := 100);

   -- Z-score (scaled by 10)
   function Z_Score (Data : Data_Array; Value : Integer) return Integer;

private

   -- Internal sorting for median/percentile
   procedure Sort (Data : in Out Data_Array);

end GNAT.Statistical;
