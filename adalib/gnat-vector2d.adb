-- GNAT.Vector2D body for Z80
-- 2D vector operations implementation

package body GNAT.Vector2D is

   -- Simple integer square root
   function Isqrt (N : Natural) return Natural is
      Guess, Prev : Natural;
   begin
      if N = 0 then
         return 0;
      end if;

      Guess := N / 2;
      if Guess = 0 then
         Guess := 1;
      end if;

      for I in 1 .. 10 loop
         Prev := Guess;
         Guess := (Guess + N / Guess) / 2;
         exit when Guess >= Prev;
      end loop;

      return Guess;
   end Isqrt;

   -- Simple sine approximation (angle in degrees, result * 100)
   function Sin_100 (Angle : Integer) return Integer is
      A : Integer := Angle mod 360;
      Sign : Integer := 1;
      Result : Integer;
   begin
      if A < 0 then
         A := A + 360;
      end if;

      if A > 180 then
         Sign := -1;
         A := A - 180;
      end if;

      if A > 90 then
         A := 180 - A;
      end if;

      -- Approximate sin using polynomial: sin(x) ≈ x - x³/6 (for small x)
      -- But for simplicity, use lookup-like calculation
      -- sin(0) = 0, sin(30) = 50, sin(45) = 71, sin(60) = 87, sin(90) = 100
      if A <= 30 then
         Result := (A * 50) / 30;
      elsif A <= 45 then
         Result := 50 + ((A - 30) * 21) / 15;
      elsif A <= 60 then
         Result := 71 + ((A - 45) * 16) / 15;
      elsif A <= 90 then
         Result := 87 + ((A - 60) * 13) / 30;
      else
         Result := 100;
      end if;

      return Sign * Result;
   end Sin_100;

   function Cos_100 (Angle : Integer) return Integer is
   begin
      return Sin_100 (90 - Angle);
   end Cos_100;

   ----------
   -- Make --
   ----------

   function Make (X, Y : Integer) return Vector2 is
   begin
      return (X => X, Y => Y);
   end Make;

   ----------------
   -- From_Angle --
   ----------------

   function From_Angle (Angle_Deg : Integer; Length : Positive := 100) return Vector2 is
   begin
      return (X => (Cos_100 (Angle_Deg) * Length) / 100,
              Y => (Sin_100 (Angle_Deg) * Length) / 100);
   end From_Angle;

   ---------
   -- "+" --
   ---------

   function "+" (A, B : Vector2) return Vector2 is
   begin
      return (X => A.X + B.X, Y => A.Y + B.Y);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (A, B : Vector2) return Vector2 is
   begin
      return (X => A.X - B.X, Y => A.Y - B.Y);
   end "-";

   function "-" (V : Vector2) return Vector2 is
   begin
      return (X => -V.X, Y => -V.Y);
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (V : Vector2; S : Integer) return Vector2 is
   begin
      return (X => V.X * S, Y => V.Y * S);
   end "*";

   function "*" (S : Integer; V : Vector2) return Vector2 is
   begin
      return V * S;
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (V : Vector2; S : Integer) return Vector2 is
   begin
      if S = 0 then
         return Zero_Vector;
      end if;
      return (X => V.X / S, Y => V.Y / S);
   end "/";

   ---------
   -- "=" --
   ---------

   function "=" (A, B : Vector2) return Boolean is
   begin
      return A.X = B.X and A.Y = B.Y;
   end "=";

   ---------
   -- Dot --
   ---------

   function Dot (A, B : Vector2) return Integer is
   begin
      return A.X * B.X + A.Y * B.Y;
   end Dot;

   -----------
   -- Cross --
   -----------

   function Cross (A, B : Vector2) return Integer is
   begin
      return A.X * B.Y - A.Y * B.X;
   end Cross;

   --------------------
   -- Length_Squared --
   --------------------

   function Length_Squared (V : Vector2) return Integer is
   begin
      return V.X * V.X + V.Y * V.Y;
   end Length_Squared;

   ------------
   -- Length --
   ------------

   function Length (V : Vector2) return Integer is
   begin
      return Isqrt (Natural (abs Length_Squared (V)));
   end Length;

   ----------------------
   -- Distance_Squared --
   ----------------------

   function Distance_Squared (A, B : Vector2) return Integer is
   begin
      return Length_Squared (B - A);
   end Distance_Squared;

   --------------
   -- Distance --
   --------------

   function Distance (A, B : Vector2) return Integer is
   begin
      return Length (B - A);
   end Distance;

   ------------------------
   -- Manhattan_Distance --
   ------------------------

   function Manhattan_Distance (A, B : Vector2) return Integer is
   begin
      return abs (B.X - A.X) + abs (B.Y - A.Y);
   end Manhattan_Distance;

   ---------------
   -- Normalize --
   ---------------

   function Normalize (V : Vector2) return Vector2 is
   begin
      return Normalize (V, 100);
   end Normalize;

   function Normalize (V : Vector2; Target_Length : Positive) return Vector2 is
      L : constant Integer := Length (V);
   begin
      if L = 0 then
         return Zero_Vector;
      end if;
      return (X => (V.X * Target_Length) / L,
              Y => (V.Y * Target_Length) / L);
   end Normalize;

   -----------
   -- Angle --
   -----------

   function Angle (V : Vector2) return Integer is
      -- Returns angle in degrees * 100
      -- Using atan2 approximation
      AX : constant Integer := abs V.X;
      AY : constant Integer := abs V.Y;
      Ang : Integer;
   begin
      if V.X = 0 and V.Y = 0 then
         return 0;
      end if;

      -- Approximate atan using simple ratio
      if AX >= AY then
         if AX > 0 then
            Ang := (AY * 4500) / AX;  -- 0-45 degrees * 100
         else
            Ang := 0;
         end if;
      else
         Ang := 9000 - (AX * 4500) / AY;  -- 45-90 degrees * 100
      end if;

      -- Adjust for quadrant
      if V.X < 0 then
         Ang := 18000 - Ang;
      end if;
      if V.Y < 0 then
         Ang := -Ang;
      end if;

      return Ang;
   end Angle;

   -------------------
   -- Angle_Between --
   -------------------

   function Angle_Between (A, B : Vector2) return Integer is
      D : constant Integer := Dot (A, B);
      LA : constant Integer := Length (A);
      LB : constant Integer := Length (B);
   begin
      if LA = 0 or LB = 0 then
         return 0;
      end if;
      -- cos(angle) = dot / (|A| * |B|)
      -- Using approximate inverse cosine
      declare
         Cos_Val : constant Integer := (D * 100) / (LA * LB / 100);
      begin
         -- Approximate acos: linear interpolation
         if Cos_Val >= 100 then
            return 0;
         elsif Cos_Val <= -100 then
            return 18000;
         else
            return ((100 - Cos_Val) * 9000) / 200;
         end if;
      end;
   end Angle_Between;

   ------------
   -- Rotate --
   ------------

   function Rotate (V : Vector2; Angle_Deg : Integer) return Vector2 is
      C : constant Integer := Cos_100 (Angle_Deg);
      S : constant Integer := Sin_100 (Angle_Deg);
   begin
      return (X => (V.X * C - V.Y * S) / 100,
              Y => (V.X * S + V.Y * C) / 100);
   end Rotate;

   ------------------
   -- Rotate_90_CW --
   ------------------

   function Rotate_90_CW (V : Vector2) return Vector2 is
   begin
      return (X => V.Y, Y => -V.X);
   end Rotate_90_CW;

   -------------------
   -- Rotate_90_CCW --
   -------------------

   function Rotate_90_CCW (V : Vector2) return Vector2 is
   begin
      return (X => -V.Y, Y => V.X);
   end Rotate_90_CCW;

   ----------------
   -- Rotate_180 --
   ----------------

   function Rotate_180 (V : Vector2) return Vector2 is
   begin
      return -V;
   end Rotate_180;

   ---------------------
   -- Perpendicular_CW --
   ---------------------

   function Perpendicular_CW (V : Vector2) return Vector2 is
   begin
      return Rotate_90_CW (V);
   end Perpendicular_CW;

   ----------------------
   -- Perpendicular_CCW --
   ----------------------

   function Perpendicular_CCW (V : Vector2) return Vector2 is
   begin
      return Rotate_90_CCW (V);
   end Perpendicular_CCW;

   -------------
   -- Reflect --
   -------------

   function Reflect (V, Normal : Vector2) return Vector2 is
      D : constant Integer := 2 * Dot (V, Normal);
      L2 : constant Integer := Length_Squared (Normal);
   begin
      if L2 = 0 then
         return V;
      end if;
      return V - Normal * D / L2;
   end Reflect;

   -------------
   -- Project --
   -------------

   function Project (V, Onto : Vector2) return Vector2 is
      D : constant Integer := Dot (V, Onto);
      L2 : constant Integer := Length_Squared (Onto);
   begin
      if L2 = 0 then
         return Zero_Vector;
      end if;
      return Onto * D / L2;
   end Project;

   ----------
   -- Lerp --
   ----------

   function Lerp (A, B : Vector2; T : Natural) return Vector2 is
      IT : constant Integer := 100 - Integer (T);
   begin
      return (X => (A.X * IT + B.X * Integer (T)) / 100,
              Y => (A.Y * IT + B.Y * Integer (T)) / 100);
   end Lerp;

   -------------
   -- Is_Zero --
   -------------

   function Is_Zero (V : Vector2) return Boolean is
   begin
      return V.X = 0 and V.Y = 0;
   end Is_Zero;

   -----------------
   -- Is_Parallel --
   -----------------

   function Is_Parallel (A, B : Vector2) return Boolean is
   begin
      return Cross (A, B) = 0;
   end Is_Parallel;

   ----------------------
   -- Is_Perpendicular --
   ----------------------

   function Is_Perpendicular (A, B : Vector2) return Boolean is
   begin
      return Dot (A, B) = 0;
   end Is_Perpendicular;

   ------------------
   -- Clamp_Length --
   ------------------

   function Clamp_Length (V : Vector2; Max_Length : Positive) return Vector2 is
      L : constant Integer := Length (V);
   begin
      if L <= Max_Length then
         return V;
      end if;
      return Normalize (V, Max_Length);
   end Clamp_Length;

   ---------------
   -- Make_Rect --
   ---------------

   function Make_Rect (X, Y : Integer; W, H : Natural) return Rect is
   begin
      return (X => X, Y => Y, W => W, H => H);
   end Make_Rect;

   --------------
   -- Contains --
   --------------

   function Contains (R : Rect; P : Point2) return Boolean is
   begin
      return P.X >= R.X and P.X < R.X + Integer (R.W) and
             P.Y >= R.Y and P.Y < R.Y + Integer (R.H);
   end Contains;

   ----------------
   -- Intersects --
   ----------------

   function Intersects (R1, R2 : Rect) return Boolean is
   begin
      return R1.X < R2.X + Integer (R2.W) and
             R1.X + Integer (R1.W) > R2.X and
             R1.Y < R2.Y + Integer (R2.H) and
             R1.Y + Integer (R1.H) > R2.Y;
   end Intersects;

   ------------
   -- Center --
   ------------

   function Center (R : Rect) return Point2 is
   begin
      return (X => R.X + Integer (R.W) / 2, Y => R.Y + Integer (R.H) / 2);
   end Center;

   ---------------
   -- Make_Line --
   ---------------

   function Make_Line (P1, P2 : Point2) return Line_Segment is
   begin
      return (P1 => P1, P2 => P2);
   end Make_Line;

   function Length (L : Line_Segment) return Integer is
   begin
      return Distance (L.P1, L.P2);
   end Length;

   --------------
   -- Midpoint --
   --------------

   function Midpoint (L : Line_Segment) return Point2 is
   begin
      return Lerp (L.P1, L.P2, 50);
   end Midpoint;

   ---------------
   -- Direction --
   ---------------

   function Direction (L : Line_Segment) return Vector2 is
   begin
      return Normalize (L.P2 - L.P1);
   end Direction;

   -------------------
   -- Point_On_Line --
   -------------------

   function Point_On_Line (L : Line_Segment; T : Natural) return Point2 is
   begin
      return Lerp (L.P1, L.P2, T);
   end Point_On_Line;

   ----------------------
   -- Distance_To_Line --
   ----------------------

   function Distance_To_Line (P : Point2; L : Line_Segment) return Integer is
      V : constant Vector2 := L.P2 - L.P1;
      W : constant Vector2 := P - L.P1;
      C1 : constant Integer := Dot (W, V);
      C2 : constant Integer := Dot (V, V);
      Proj : Point2;
   begin
      if C2 = 0 then
         return Distance (P, L.P1);
      end if;

      if C1 <= 0 then
         return Distance (P, L.P1);
      elsif C1 >= C2 then
         return Distance (P, L.P2);
      else
         Proj := L.P1 + V * C1 / C2;
         return Distance (P, Proj);
      end if;
   end Distance_To_Line;

   -------------------
   -- Closest_Point --
   -------------------

   function Closest_Point (P : Point2; L : Line_Segment) return Point2 is
      V : constant Vector2 := L.P2 - L.P1;
      W : constant Vector2 := P - L.P1;
      C1 : constant Integer := Dot (W, V);
      C2 : constant Integer := Dot (V, V);
   begin
      if C2 = 0 then
         return L.P1;
      end if;

      if C1 <= 0 then
         return L.P1;
      elsif C1 >= C2 then
         return L.P2;
      else
         return L.P1 + V * C1 / C2;
      end if;
   end Closest_Point;

   ----------------------
   -- Triangle_Area_2x --
   ----------------------

   function Triangle_Area_2x (A, B, C : Point2) return Integer is
   begin
      return (B.X - A.X) * (C.Y - A.Y) - (C.X - A.X) * (B.Y - A.Y);
   end Triangle_Area_2x;

   -----------------
   -- In_Triangle --
   -----------------

   function In_Triangle (P, A, B, C : Point2) return Boolean is
      D1 : constant Integer := Triangle_Area_2x (P, A, B);
      D2 : constant Integer := Triangle_Area_2x (P, B, C);
      D3 : constant Integer := Triangle_Area_2x (P, C, A);
      Has_Neg : constant Boolean := D1 < 0 or D2 < 0 or D3 < 0;
      Has_Pos : constant Boolean := D1 > 0 or D2 > 0 or D3 > 0;
   begin
      return not (Has_Neg and Has_Pos);
   end In_Triangle;

   ---------------
   -- In_Circle --
   ---------------

   function In_Circle (P, Center : Point2; Radius : Positive) return Boolean is
   begin
      return Distance_Squared (P, Center) <= Radius * Radius;
   end In_Circle;

end GNAT.Vector2D;
