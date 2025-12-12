-- GNAT.String_Builder for Z80
-- Efficient string building with preallocated buffer

package GNAT.String_Builder is
   pragma Preelaborate;

   Max_Capacity : constant := 512;  -- Maximum builder size for Z80

   type Builder is private;

   function Create (Capacity : Positive := 256) return Builder;
   --  Create builder with specified capacity

   function Length (B : Builder) return Natural;
   --  Return current length

   function Capacity (B : Builder) return Positive;
   --  Return maximum capacity

   function Available (B : Builder) return Natural;
   --  Return remaining space

   function Is_Empty (B : Builder) return Boolean;
   --  Check if builder is empty

   function Is_Full (B : Builder) return Boolean;
   --  Check if builder is full

   procedure Clear (B : out Builder);
   --  Clear contents

   procedure Append (B : in Out Builder; S : String);
   --  Append string

   procedure Append (B : in Out Builder; C : Character);
   --  Append character

   procedure Append (B : in Out Builder; N : Integer);
   --  Append integer as string

   procedure Append (B : in Out Builder; N : Natural; Width : Positive;
                     Fill : Character := '0');
   --  Append natural with specified width and fill

   procedure Append_Line (B : in Out Builder; S : String);
   --  Append string followed by newline

   procedure New_Line (B : in Out Builder);
   --  Append newline

   procedure Append_Hex (B : in Out Builder; N : Natural);
   --  Append natural as hexadecimal

   function To_String (B : Builder) return String;
   --  Convert to standard string

   function Last_Char (B : Builder) return Character;
   --  Return last character (NUL if empty)

   procedure Delete_Last (B : in Out Builder; Count : Natural := 1);
   --  Delete last N characters

   procedure Insert (B : in Out Builder; Before : Positive; S : String);
   --  Insert string at position

   procedure Replace (B : in Out Builder; Low, High : Positive; S : String);
   --  Replace range with string

   function Index (B : Builder; Pattern : String) return Natural;
   --  Find pattern in builder

   function Substring (B : Builder; Low, High : Positive) return String;
   --  Extract substring

   -- Formatted output helpers
   procedure Put (B : in Out Builder; S : String);
   procedure Put (B : in Out Builder; C : Character);
   procedure Put (B : in Out Builder; N : Integer);
   procedure Put_Line (B : in Out Builder; S : String := "");

private

   type String_Data is array (1 .. Max_Capacity) of Character;

   type Builder is record
      Data : String_Data := (others => ' ');
      Len  : Natural := 0;
      Cap  : Positive := 256;
   end record;

end GNAT.String_Builder;
