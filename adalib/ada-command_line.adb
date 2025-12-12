-- Ada.Command_Line body for Z80/CP/M
-- Command line argument parsing implementation

with System;

package body Ada.Command_Line is

   -- CP/M command line buffer address
   CPM_CMD_LENGTH : constant System.Address := 16#0080#;
   CPM_CMD_BUFFER : constant System.Address := 16#0081#;

   -- Cached argument information
   type Arg_Info is record
      Start  : Natural := 0;
      Length : Natural := 0;
   end record;

   Max_Args : constant := 16;
   Args : array (0 .. Max_Args) of Arg_Info;
   Num_Args : Natural := 0;
   Parsed : Boolean := False;

   -- Exit status storage
   Current_Exit_Status : Exit_Status := Success;

   -- Parse the command line into arguments
   procedure Parse_Command_Line is
      Length_Byte : Character;
      for Length_Byte'Address use CPM_CMD_LENGTH;

      Buffer : String (1 .. 127);
      for Buffer'Address use CPM_CMD_BUFFER;

      Cmd_Length : Natural;
      Pos : Natural := 1;
      Arg_Start : Natural;
      In_Arg : Boolean := False;
   begin
      if Parsed then
         return;
      end if;

      Cmd_Length := Character'Pos (Length_Byte);
      if Cmd_Length > 127 then
         Cmd_Length := 127;
      end if;

      Num_Args := 0;

      -- Parse arguments (space-separated)
      while Pos <= Cmd_Length and Num_Args < Max_Args loop
         if Buffer (Pos) = ' ' or Buffer (Pos) = Character'Val (9) then
            -- Whitespace
            if In_Arg then
               -- End of argument
               Args (Num_Args).Length := Pos - Args (Num_Args).Start;
               In_Arg := False;
            end if;
         else
            -- Non-whitespace
            if not In_Arg then
               -- Start of new argument
               Num_Args := Num_Args + 1;
               Args (Num_Args).Start := Pos;
               In_Arg := True;
            end if;
         end if;
         Pos := Pos + 1;
      end loop;

      -- Handle last argument if still in one
      if In_Arg then
         Args (Num_Args).Length := Pos - Args (Num_Args).Start;
      end if;

      Parsed := True;
   end Parse_Command_Line;

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count return Natural is
   begin
      Parse_Command_Line;
      return Num_Args;
   end Argument_Count;

   --------------
   -- Argument --
   --------------

   function Argument (Number : Positive) return String is
      Buffer : String (1 .. 127);
      for Buffer'Address use CPM_CMD_BUFFER;
   begin
      Parse_Command_Line;

      if Number > Num_Args then
         return "";
      end if;

      return Buffer (Args (Number).Start .. Args (Number).Start + Args (Number).Length - 1);
   end Argument;

   ------------------
   -- Command_Name --
   ------------------

   function Command_Name return String is
   begin
      -- CP/M doesn't provide the command name easily
      -- Return a placeholder
      return "PROGRAM";
   end Command_Name;

   ---------------------
   -- Set_Exit_Status --
   ---------------------

   procedure Set_Exit_Status (Code : Exit_Status) is
   begin
      Current_Exit_Status := Code;
   end Set_Exit_Status;

end Ada.Command_Line;
