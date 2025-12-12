-- Ada.Strings.Superbounded for Z80
-- Super bounded string support

with Ada.Strings.Maps;

package Ada.Strings.Superbounded is
   pragma Preelaborate;

   Max_Length : constant := 255;
   --  Maximum string length for Z80

   type Super_String is record
      Max_Length : Positive;
      Cur_Length : Natural;
      Data       : String (1 .. Max_Length);
   end record;

   function Length (Source : Super_String) return Natural;
   --  Return current length

   procedure Set_Super_String
     (Target   : in Out Super_String;
      Source   : String;
      Max      : Positive;
      Drop     : Truncation := Error);
   --  Set super string from string

   function Super_To_String (Source : Super_String) return String;
   --  Convert to regular string

   function Super_Length (Source : Super_String) return Natural
     renames Length;

   function Super_Element
     (Source : Super_String;
      Index  : Positive) return Character;
   --  Return character at index

   procedure Super_Replace_Element
     (Source : in Out Super_String;
      Index  : Positive;
      By     : Character);
   --  Replace character at index

   function Super_Slice
     (Source : Super_String;
      Low    : Positive;
      High   : Natural) return String;
   --  Return slice

   function Super_Append
     (Left  : Super_String;
      Right : Super_String;
      Drop  : Truncation := Error) return Super_String;
   --  Append two super strings

   function Super_Append
     (Left  : Super_String;
      Right : String;
      Drop  : Truncation := Error) return Super_String;
   --  Append string to super string

   function Super_Index
     (Source  : Super_String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;
   --  Find pattern in super string

end Ada.Strings.Superbounded;
