-- GNAT.Math body for Z80
-- Basic math utilities implementation

package body GNAT.Math is

   ----------
   -- Sign --
   ----------

   function Sign (X : Integer) return Integer is
   begin
      if X > 0 then
         return 1;
      elsif X < 0 then
         return -1;
      else
         return 0;
      end if;
   end Sign;

   ----------
   -- Sign --
   ----------

   function Sign (X : Float) return Float is
   begin
      if X > 0.0 then
         return 1.0;
      elsif X < 0.0 then
         return -1.0;
      else
         return 0.0;
      end if;
   end Sign;

   ---------------
   -- Abs_Value --
   ---------------

   function Abs_Value (X : Integer) return Integer is
   begin
      if X < 0 then
         return -X;
      else
         return X;
      end if;
   end Abs_Value;

   ---------------
   -- Abs_Value --
   ---------------

   function Abs_Value (X : Float) return Float is
   begin
      if X < 0.0 then
         return -X;
      else
         return X;
      end if;
   end Abs_Value;

   ---------
   -- Min --
   ---------

   function Min (A, B : Integer) return Integer is
   begin
      if A <= B then
         return A;
      else
         return B;
      end if;
   end Min;

   ---------
   -- Max --
   ---------

   function Max (A, B : Integer) return Integer is
   begin
      if A >= B then
         return A;
      else
         return B;
      end if;
   end Max;

   ---------
   -- Min --
   ---------

   function Min (A, B : Float) return Float is
   begin
      if A <= B then
         return A;
      else
         return B;
      end if;
   end Min;

   ---------
   -- Max --
   ---------

   function Max (A, B : Float) return Float is
   begin
      if A >= B then
         return A;
      else
         return B;
      end if;
   end Max;

   -----------
   -- Clamp --
   -----------

   function Clamp
     (Value : Integer;
      Low   : Integer;
      High  : Integer) return Integer
   is
   begin
      if Value < Low then
         return Low;
      elsif Value > High then
         return High;
      else
         return Value;
      end if;
   end Clamp;

   -----------
   -- Clamp --
   -----------

   function Clamp
     (Value : Float;
      Low   : Float;
      High  : Float) return Float
   is
   begin
      if Value < Low then
         return Low;
      elsif Value > High then
         return High;
      else
         return Value;
      end if;
   end Clamp;

   ---------
   -- GCD --
   ---------

   function GCD (A, B : Natural) return Natural is
      X : Natural := A;
      Y : Natural := B;
      T : Natural;
   begin
      while Y /= 0 loop
         T := Y;
         Y := X mod Y;
         X := T;
      end loop;
      return X;
   end GCD;

   ---------
   -- LCM --
   ---------

   function LCM (A, B : Positive) return Positive is
   begin
      return (A / GCD (A, B)) * B;
   end LCM;

end GNAT.Math;
