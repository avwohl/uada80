-- GNAT.Vector2D for Z80
-- 2D vector and point operations using integer arithmetic

package GNAT.Vector2D is
   pragma Pure;

   -- Basic 2D vector using integers
   type Vector2 is record
      X : Integer;
      Y : Integer;
   end record;

   -- Alias for point
   subtype Point2 is Vector2;

   -- Zero and unit vectors
   Zero_Vector : constant Vector2 := (0, 0);
   Unit_X      : constant Vector2 := (1, 0);
   Unit_Y      : constant Vector2 := (0, 1);

   -- Constructors
   function Make (X, Y : Integer) return Vector2;
   function From_Angle (Angle_Deg : Integer; Length : Positive := 100) return Vector2;
   -- Angle in degrees, length scaled (100 = 1.0)

   -- Basic operations
   function "+" (A, B : Vector2) return Vector2;
   function "-" (A, B : Vector2) return Vector2;
   function "-" (V : Vector2) return Vector2;  -- Negation
   function "*" (V : Vector2; S : Integer) return Vector2;  -- Scale
   function "*" (S : Integer; V : Vector2) return Vector2;  -- Scale
   function "/" (V : Vector2; S : Integer) return Vector2;  -- Scale down

   -- Equality
   function "=" (A, B : Vector2) return Boolean;

   -- Dot and cross products
   function Dot (A, B : Vector2) return Integer;
   function Cross (A, B : Vector2) return Integer;  -- Z component of 3D cross

   -- Length operations (using scaled integers)
   function Length_Squared (V : Vector2) return Integer;
   function Length (V : Vector2) return Integer;  -- Approximate using integer sqrt

   -- Distance operations
   function Distance_Squared (A, B : Vector2) return Integer;
   function Distance (A, B : Vector2) return Integer;

   -- Manhattan distance (faster, no sqrt)
   function Manhattan_Distance (A, B : Vector2) return Integer;

   -- Normalization (scales to length 100)
   function Normalize (V : Vector2) return Vector2;
   function Normalize (V : Vector2; Target_Length : Positive) return Vector2;

   -- Angle operations (returns degrees * 100 for precision)
   function Angle (V : Vector2) return Integer;
   function Angle_Between (A, B : Vector2) return Integer;

   -- Rotation (angle in degrees)
   function Rotate (V : Vector2; Angle_Deg : Integer) return Vector2;
   function Rotate_90_CW (V : Vector2) return Vector2;
   function Rotate_90_CCW (V : Vector2) return Vector2;
   function Rotate_180 (V : Vector2) return Vector2;

   -- Perpendicular vectors
   function Perpendicular_CW (V : Vector2) return Vector2;
   function Perpendicular_CCW (V : Vector2) return Vector2;

   -- Reflection
   function Reflect (V, Normal : Vector2) return Vector2;

   -- Projection
   function Project (V, Onto : Vector2) return Vector2;

   -- Lerp (interpolation, T is 0-100 representing 0.0-1.0)
   function Lerp (A, B : Vector2; T : Natural) return Vector2;

   -- Comparison
   function Is_Zero (V : Vector2) return Boolean;
   function Is_Parallel (A, B : Vector2) return Boolean;
   function Is_Perpendicular (A, B : Vector2) return Boolean;

   -- Clamping
   function Clamp_Length (V : Vector2; Max_Length : Positive) return Vector2;

   -- Bounding box
   type Rect is record
      X, Y : Integer;  -- Top-left corner
      W, H : Natural;  -- Width and height
   end record;

   function Make_Rect (X, Y : Integer; W, H : Natural) return Rect;
   function Contains (R : Rect; P : Point2) return Boolean;
   function Intersects (R1, R2 : Rect) return Boolean;
   function Center (R : Rect) return Point2;

   -- Line segment
   type Line_Segment is record
      P1, P2 : Point2;
   end record;

   function Make_Line (P1, P2 : Point2) return Line_Segment;
   function Length (L : Line_Segment) return Integer;
   function Midpoint (L : Line_Segment) return Point2;
   function Direction (L : Line_Segment) return Vector2;

   -- Point on line (T is 0-100)
   function Point_On_Line (L : Line_Segment; T : Natural) return Point2;

   -- Distance from point to line
   function Distance_To_Line (P : Point2; L : Line_Segment) return Integer;

   -- Closest point on line segment to point
   function Closest_Point (P : Point2; L : Line_Segment) return Point2;

   -- Triangle area (scaled by 2, can be negative for orientation)
   function Triangle_Area_2x (A, B, C : Point2) return Integer;

   -- Point in triangle test
   function In_Triangle (P, A, B, C : Point2) return Boolean;

   -- Circle-related
   function In_Circle (P, Center : Point2; Radius : Positive) return Boolean;

end GNAT.Vector2D;
