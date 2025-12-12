-- GNAT.Command_Line body for Z80
-- Command line argument parsing implementation

with Ada.Command_Line;

package body GNAT.Command_Line is

   Current_Arg   : Natural := 1;
   Current_Pos   : Natural := 1;
   Last_Switch   : String (1 .. 64);
   Last_Length   : Natural := 0;
   Last_Param    : String (1 .. 128);
   Param_Length  : Natural := 0;

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count return Natural is
   begin
      return Ada.Command_Line.Argument_Count;
   end Argument_Count;

   --------------
   -- Argument --
   --------------

   function Argument (Index : Positive) return String is
   begin
      return Ada.Command_Line.Argument (Index);
   end Argument;

   ------------------
   -- Command_Name --
   ------------------

   function Command_Name return String is
   begin
      return Ada.Command_Line.Command_Name;
   end Command_Name;

   -------------------
   -- Define_Switch --
   -------------------

   procedure Define_Switch
     (Config      : in Out Command_Line_Configuration;
      Switch      : String;
      Help        : String := "")
   is
      pragma Unreferenced (Help);
   begin
      if Config.Count < Max_Switches then
         Config.Count := Config.Count + 1;
         Config.Switches (Config.Count).Length :=
           Natural'Min (Switch'Length, 16);
         Config.Switches (Config.Count).Switch
           (1 .. Config.Switches (Config.Count).Length) :=
           Switch (Switch'First ..
                   Switch'First + Config.Switches (Config.Count).Length - 1);
      end if;
   end Define_Switch;

   ------------
   -- Getopt --
   ------------

   function Getopt
     (Config : Command_Line_Configuration;
      Parser : String := "") return Character
   is
      pragma Unreferenced (Config, Parser);
      Arg : constant String := (if Current_Arg <= Argument_Count
                                then Argument (Current_Arg)
                                else "");
   begin
      -- Reset param
      Param_Length := 0;

      -- No more arguments
      if Current_Arg > Argument_Count then
         return ASCII.NUL;
      end if;

      -- Check if it's a switch
      if Arg'Length > 0 and then Arg (Arg'First) = '-' then
         Last_Length := Arg'Length;
         Last_Switch (1 .. Last_Length) := Arg;

         -- Check for parameter (e.g., -o file or -ofile)
         if Arg'Length > 2 and then Arg (Arg'First + 1) /= '-' then
            -- Single letter option, rest might be parameter
            Param_Length := Arg'Length - 2;
            Last_Param (1 .. Param_Length) :=
              Arg (Arg'First + 2 .. Arg'Last);
         end if;

         Current_Arg := Current_Arg + 1;

         -- Return the option character
         if Arg'Length >= 2 then
            return Arg (Arg'First + 1);
         else
            return '-';
         end if;
      else
         -- Not a switch
         return ASCII.NUL;
      end if;
   end Getopt;

   ---------------
   -- Parameter --
   ---------------

   function Parameter return String is
   begin
      if Param_Length > 0 then
         return Last_Param (1 .. Param_Length);
      elsif Current_Arg <= Argument_Count then
         declare
            Arg : constant String := Argument (Current_Arg);
         begin
            if Arg'Length = 0 or else Arg (Arg'First) /= '-' then
               Current_Arg := Current_Arg + 1;
               return Arg;
            end if;
         end;
      end if;
      return "";
   end Parameter;

   -----------------
   -- Full_Switch --
   -----------------

   function Full_Switch return String is
   begin
      return Last_Switch (1 .. Last_Length);
   end Full_Switch;

   ------------------
   -- Get_Argument --
   ------------------

   function Get_Argument return String is
   begin
      if Current_Arg <= Argument_Count then
         declare
            Arg : constant String := Argument (Current_Arg);
         begin
            Current_Arg := Current_Arg + 1;
            return Arg;
         end;
      end if;
      return "";
   end Get_Argument;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Current_Arg := 1;
      Current_Pos := 1;
      Last_Length := 0;
      Param_Length := 0;
   end Reset;

end GNAT.Command_Line;
