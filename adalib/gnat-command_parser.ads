-- GNAT.Command_Parser for Z80
-- Simple command line argument parser

package GNAT.Command_Parser is
   pragma Preelaborate;

   Max_Arguments : constant := 16;  -- Maximum arguments for Z80
   Max_Arg_Length : constant := 64;
   Max_Options : constant := 16;

   type Parser is private;

   procedure Initialize (P : out Parser);
   --  Initialize parser

   procedure Parse (P : in Out Parser; Command_Line : String);
   --  Parse a command line string

   function Argument_Count (P : Parser) return Natural;
   --  Return number of arguments (excluding program name)

   function Argument (P : Parser; Index : Positive) return String;
   --  Get argument by index (1-based)

   function Program_Name (P : Parser) return String;
   --  Get program name (argument 0)

   -- Option support
   procedure Define_Option
     (P         : in Out Parser;
      Short     : Character;
      Long      : String;
      Has_Value : Boolean := False);
   --  Define an option (e.g., -v, --verbose)

   function Option_Present (P : Parser; Short : Character) return Boolean;
   function Option_Present (P : Parser; Long : String) return Boolean;
   --  Check if option was specified

   function Option_Value (P : Parser; Short : Character) return String;
   function Option_Value (P : Parser; Long : String) return String;
   --  Get option value (empty if not present or no value)

   function Non_Option_Count (P : Parser) return Natural;
   --  Return number of non-option arguments

   function Non_Option (P : Parser; Index : Positive) return String;
   --  Get non-option argument by index

   function Has_Error (P : Parser) return Boolean;
   --  Check if parsing error occurred

   function Error_Message (P : Parser) return String;
   --  Get error message

private

   type Arg_String is array (1 .. Max_Arg_Length) of Character;

   type Argument_Entry is record
      Data   : Arg_String := (others => ' ');
      Length : Natural := 0;
      Is_Option : Boolean := False;
   end record;

   type Argument_Array is array (0 .. Max_Arguments) of Argument_Entry;

   type Option_Entry is record
      Short     : Character := ASCII.NUL;
      Long      : Arg_String := (others => ' ');
      Long_Len  : Natural := 0;
      Has_Value : Boolean := False;
      Present   : Boolean := False;
      Value     : Arg_String := (others => ' ');
      Value_Len : Natural := 0;
   end record;

   type Option_Array is array (1 .. Max_Options) of Option_Entry;

   type Parser is record
      Args        : Argument_Array;
      Arg_Count   : Natural := 0;
      Options     : Option_Array;
      Opt_Count   : Natural := 0;
      Has_Err     : Boolean := False;
      Err_Msg     : Arg_String := (others => ' ');
      Err_Len     : Natural := 0;
   end record;

end GNAT.Command_Parser;
