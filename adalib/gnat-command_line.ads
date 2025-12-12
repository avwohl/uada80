-- GNAT.Command_Line for Z80
-- Command line argument parsing

package GNAT.Command_Line is
   pragma Preelaborate;

   -- Get command line arguments
   function Argument_Count return Natural;
   function Argument (Index : Positive) return String;
   function Command_Name return String;

   -- Option parsing
   type Command_Line_Configuration is limited private;

   Invalid_Switch    : exception;
   Invalid_Parameter : exception;

   -- Define switches
   procedure Define_Switch
     (Config      : in Out Command_Line_Configuration;
      Switch      : String;
      Help        : String := "");

   -- Get option
   function Getopt
     (Config : Command_Line_Configuration;
      Parser : String := "") return Character;

   -- Get parameter after option
   function Parameter return String;

   -- Current switch being processed
   function Full_Switch return String;

   -- Free form arguments
   function Get_Argument return String;

   -- Reset for re-parsing
   procedure Reset;

private

   Max_Switches : constant := 16;

   type Switch_Info is record
      Switch : String (1 .. 16);
      Length : Natural := 0;
   end record;

   type Command_Line_Configuration is limited record
      Switches : array (1 .. Max_Switches) of Switch_Info;
      Count    : Natural := 0;
   end record;

end GNAT.Command_Line;
