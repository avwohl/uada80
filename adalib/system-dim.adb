-- System.Dim body for Z80
-- Dimension system support implementation

package body System.Dim is

   --------------
   -- Multiply --
   --------------

   function Multiply (Left, Right : Dimension_Type) return Dimension_Type is
   begin
      return (Length            => Left.Length + Right.Length,
              Mass              => Left.Mass + Right.Mass,
              Time              => Left.Time + Right.Time,
              Electric_Current  => Left.Electric_Current + Right.Electric_Current,
              Temperature       => Left.Temperature + Right.Temperature,
              Amount_Of_Substance => Left.Amount_Of_Substance + Right.Amount_Of_Substance,
              Luminous_Intensity => Left.Luminous_Intensity + Right.Luminous_Intensity);
   end Multiply;

   ------------
   -- Divide --
   ------------

   function Divide (Left, Right : Dimension_Type) return Dimension_Type is
   begin
      return (Length            => Left.Length - Right.Length,
              Mass              => Left.Mass - Right.Mass,
              Time              => Left.Time - Right.Time,
              Electric_Current  => Left.Electric_Current - Right.Electric_Current,
              Temperature       => Left.Temperature - Right.Temperature,
              Amount_Of_Substance => Left.Amount_Of_Substance - Right.Amount_Of_Substance,
              Luminous_Intensity => Left.Luminous_Intensity - Right.Luminous_Intensity);
   end Divide;

   -----------
   -- Power --
   -----------

   function Power (D : Dimension_Type; N : Integer) return Dimension_Type is
   begin
      return (Length            => D.Length * Dimension_Exponent (N),
              Mass              => D.Mass * Dimension_Exponent (N),
              Time              => D.Time * Dimension_Exponent (N),
              Electric_Current  => D.Electric_Current * Dimension_Exponent (N),
              Temperature       => D.Temperature * Dimension_Exponent (N),
              Amount_Of_Substance => D.Amount_Of_Substance * Dimension_Exponent (N),
              Luminous_Intensity => D.Luminous_Intensity * Dimension_Exponent (N));
   end Power;

   ----------
   -- Root --
   ----------

   function Root (D : Dimension_Type; N : Positive) return Dimension_Type is
   begin
      -- Note: This only works when dimensions are evenly divisible
      return (Length            => D.Length / Dimension_Exponent (N),
              Mass              => D.Mass / Dimension_Exponent (N),
              Time              => D.Time / Dimension_Exponent (N),
              Electric_Current  => D.Electric_Current / Dimension_Exponent (N),
              Temperature       => D.Temperature / Dimension_Exponent (N),
              Amount_Of_Substance => D.Amount_Of_Substance / Dimension_Exponent (N),
              Luminous_Intensity => D.Luminous_Intensity / Dimension_Exponent (N));
   end Root;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Dimension_Type) return Boolean is
   begin
      return Left.Length = Right.Length and
             Left.Mass = Right.Mass and
             Left.Time = Right.Time and
             Left.Electric_Current = Right.Electric_Current and
             Left.Temperature = Right.Temperature and
             Left.Amount_Of_Substance = Right.Amount_Of_Substance and
             Left.Luminous_Intensity = Right.Luminous_Intensity;
   end "=";

end System.Dim;
