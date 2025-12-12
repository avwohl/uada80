-- GNAT.Bounded_Strings for Z80
-- Fixed-capacity bounded strings

package GNAT.Bounded_Strings is
   pragma Pure;

   Max_Length : constant := 255;  -- Maximum string length for Z80

   type Bounded_String is private;

   Empty_String : constant Bounded_String;

   function Length (Source : Bounded_String) return Natural;
   --  Return current length

   function Max_Size (Source : Bounded_String) return Natural;
   --  Return maximum capacity

   function To_String (Source : Bounded_String) return String;
   --  Convert to standard string

   function To_Bounded_String (Source : String) return Bounded_String;
   --  Convert from standard string (truncates if needed)

   procedure Set (Target : out Bounded_String; Source : String);
   --  Set bounded string from standard string

   procedure Append (Source : in out Bounded_String; New_Item : String);
   --  Append string

   procedure Append (Source : in Out Bounded_String; New_Item : Character);
   --  Append character

   procedure Append (Source : in Out Bounded_String; New_Item : Bounded_String);
   --  Append bounded string

   function "&" (Left, Right : Bounded_String) return Bounded_String;
   function "&" (Left : Bounded_String; Right : String) return Bounded_String;
   function "&" (Left : String; Right : Bounded_String) return Bounded_String;
   function "&" (Left : Bounded_String; Right : Character) return Bounded_String;

   function Element (Source : Bounded_String; Index : Positive) return Character;
   --  Get character at index

   procedure Replace_Element
     (Source : in Out Bounded_String;
      Index  : Positive;
      By     : Character);
   --  Replace character at index

   function Slice
     (Source : Bounded_String;
      Low    : Positive;
      High   : Natural) return String;
   --  Extract slice as standard string

   procedure Clear (Source : out Bounded_String);
   --  Clear string to empty

   function "=" (Left, Right : Bounded_String) return Boolean;
   function "<" (Left, Right : Bounded_String) return Boolean;
   function ">" (Left, Right : Bounded_String) return Boolean;
   function "<=" (Left, Right : Bounded_String) return Boolean;
   function ">=" (Left, Right : Bounded_String) return Boolean;

   function Index
     (Source  : Bounded_String;
      Pattern : String) return Natural;
   --  Find pattern in string, return 0 if not found

   function Count
     (Source  : Bounded_String;
      Pattern : String) return Natural;
   --  Count occurrences of pattern

   procedure Trim
     (Source : in Out Bounded_String;
      Side   : Ada.Strings.Trim_End := Ada.Strings.Both);
   --  Trim whitespace

   procedure Head
     (Source : in Out Bounded_String;
      Count  : Natural);
   --  Keep only first Count characters

   procedure Tail
     (Source : in Out Bounded_String;
      Count  : Natural);
   --  Keep only last Count characters

private

   type Bounded_String is record
      Data   : String (1 .. Max_Length);
      Length : Natural := 0;
   end record;

   Empty_String : constant Bounded_String :=
     (Data => (others => ' '), Length => 0);

end GNAT.Bounded_Strings;
