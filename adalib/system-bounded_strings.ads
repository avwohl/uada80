-- System.Bounded_Strings for Z80
-- Low-level bounded string operations

package System.Bounded_Strings is
   pragma Preelaborate;

   -- Maximum string length for Z80
   Max_Length : constant := 255;

   -- Bounded string type
   type Bounded_String is record
      Length : Natural := 0;
      Data   : String (1 .. Max_Length);
   end record;

   Empty_String : constant Bounded_String :=
     (Length => 0, Data => (others => ' '));

   -- Operations
   function To_Bounded (Source : String) return Bounded_String;
   function To_String (Source : Bounded_String) return String;

   function Length (Source : Bounded_String) return Natural;

   procedure Append
     (Source : in Out Bounded_String;
      New_Item : String);

   procedure Append
     (Source : in Out Bounded_String;
      New_Item : Character);

   function Element
     (Source : Bounded_String;
      Index  : Positive) return Character;

   procedure Replace_Element
     (Source : in Out Bounded_String;
      Index  : Positive;
      By     : Character);

   function Slice
     (Source : Bounded_String;
      Low    : Positive;
      High   : Natural) return String;

   function "=" (Left, Right : Bounded_String) return Boolean;
   function "<" (Left, Right : Bounded_String) return Boolean;

end System.Bounded_Strings;
