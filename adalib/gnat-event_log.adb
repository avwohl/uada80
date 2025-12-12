-- GNAT.Event_Log body for Z80
-- Event logging implementation

package body GNAT.Event_Log is

   Current_Tick : Natural := 0;  -- Placeholder for actual tick counter

   function Get_Tick return Natural is
   begin
      Current_Tick := Current_Tick + 1;  -- Simple incrementing counter
      return Current_Tick;
   end Get_Tick;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Count := 0;
      Write_Index := 0;
      Read_Index := 1;
      Overflow := False;
      Min_Level := Debug;
      Level_Counts := (others => 0);
   end Initialize;

   ---------
   -- Log --
   ---------

   procedure Log (Level : Event_Level; ID : Event_ID; Msg : String := "") is
      E : Event_Record;
      Len : Natural;
   begin
      -- Check minimum level
      if Level < Min_Level then
         return;
      end if;

      -- Prepare event record
      E.Level := Level;
      E.ID := ID;
      E.Timestamp := Get_Tick;
      E.Message := (others => ' ');

      -- Copy message (truncate if needed)
      Len := Msg'Length;
      if Len > Max_Msg_Length then
         Len := Max_Msg_Length;
      end if;
      E.Msg_Len := Len;

      for I in 1 .. Len loop
         E.Message (I) := Msg (Msg'First + I - 1);
      end loop;

      -- Store event
      Write_Index := Write_Index + 1;
      if Write_Index > Max_Events then
         Write_Index := 1;
      end if;

      if Count = Max_Events then
         Overflow := True;
         -- Adjust read index (oldest gets overwritten)
         Read_Index := Read_Index + 1;
         if Read_Index > Max_Events then
            Read_Index := 1;
         end if;
         -- Decrement old level count
         Level_Counts (Events (Write_Index).Level) :=
           Level_Counts (Events (Write_Index).Level) - 1;
      else
         Count := Count + 1;
      end if;

      Events (Write_Index) := E;
      Level_Counts (Level) := Level_Counts (Level) + 1;

      -- Call callback if set
      if Callback /= null then
         Callback (E);
      end if;
   end Log;

   ---------------
   -- Log_Debug --
   ---------------

   procedure Log_Debug (ID : Event_ID; Msg : String := "") is
   begin
      Log (Debug, ID, Msg);
   end Log_Debug;

   --------------
   -- Log_Info --
   --------------

   procedure Log_Info (ID : Event_ID; Msg : String := "") is
   begin
      Log (Info, ID, Msg);
   end Log_Info;

   -----------------
   -- Log_Warning --
   -----------------

   procedure Log_Warning (ID : Event_ID; Msg : String := "") is
   begin
      Log (Warning, ID, Msg);
   end Log_Warning;

   ---------------
   -- Log_Error --
   ---------------

   procedure Log_Error (ID : Event_ID; Msg : String := "") is
   begin
      Log (Error, ID, Msg);
   end Log_Error;

   ------------------
   -- Log_Critical --
   ------------------

   procedure Log_Critical (ID : Event_ID; Msg : String := "") is
   begin
      Log (Critical, ID, Msg);
   end Log_Critical;

   ---------------
   -- Log_Value --
   ---------------

   procedure Log_Value (Level : Event_Level; ID : Event_ID; Value : Integer) is
      S : constant String := Integer'Image (Value);
   begin
      Log (Level, ID, S);
   end Log_Value;

   -------------------
   -- Set_Min_Level --
   -------------------

   procedure Set_Min_Level (Level : Event_Level) is
   begin
      Min_Level := Level;
   end Set_Min_Level;

   -------------------
   -- Get_Min_Level --
   -------------------

   function Get_Min_Level return Event_Level is
   begin
      return Min_Level;
   end Get_Min_Level;

   -----------------
   -- Event_Count --
   -----------------

   function Event_Count return Natural is
   begin
      return Count;
   end Event_Count;

   function Event_Count (Level : Event_Level) return Natural is
   begin
      return Level_Counts (Level);
   end Event_Count;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Count := 0;
      Write_Index := 0;
      Read_Index := 1;
      Overflow := False;
      Level_Counts := (others => 0);
   end Clear;

   -------------
   -- Is_Full --
   -------------

   function Is_Full return Boolean is
   begin
      return Count = Max_Events;
   end Is_Full;

   ------------------
   -- Has_Overflow --
   ------------------

   function Has_Overflow return Boolean is
   begin
      return Overflow;
   end Has_Overflow;

   -------------------------
   -- Clear_Overflow_Flag --
   -------------------------

   procedure Clear_Overflow_Flag is
   begin
      Overflow := False;
   end Clear_Overflow_Flag;

   ---------------
   -- Get_Event --
   ---------------

   function Get_Event (Index : Positive) return Event_Record is
      Actual_Index : Natural;
   begin
      if Index > Count then
         return (Level => Debug, ID => 0, Timestamp => 0,
                 Message => (others => ' '), Msg_Len => 0);
      end if;

      Actual_Index := Read_Index + Index - 1;
      if Actual_Index > Max_Events then
         Actual_Index := Actual_Index - Max_Events;
      end if;

      return Events (Actual_Index);
   end Get_Event;

   ----------------
   -- Get_Latest --
   ----------------

   function Get_Latest return Event_Record is
   begin
      if Count = 0 then
         return (Level => Debug, ID => 0, Timestamp => 0,
                 Message => (others => ' '), Msg_Len => 0);
      end if;
      return Events (Write_Index);
   end Get_Latest;

   ----------------
   -- Get_Oldest --
   ----------------

   function Get_Oldest return Event_Record is
   begin
      if Count = 0 then
         return (Level => Debug, ID => 0, Timestamp => 0,
                 Message => (others => ' '), Msg_Len => 0);
      end if;
      return Events (Read_Index);
   end Get_Oldest;

   -------------------
   -- Find_By_Level --
   -------------------

   function Find_By_Level (Level : Event_Level; Start : Positive := 1) return Natural is
   begin
      for I in Start .. Count loop
         if Get_Event (I).Level = Level then
            return I;
         end if;
      end loop;
      return 0;
   end Find_By_Level;

   ----------------
   -- Find_By_ID --
   ----------------

   function Find_By_ID (ID : Event_ID; Start : Positive := 1) return Natural is
   begin
      for I in Start .. Count loop
         if Get_Event (I).ID = ID then
            return I;
         end if;
      end loop;
      return 0;
   end Find_By_ID;

   ------------------
   -- Set_Callback --
   ------------------

   procedure Set_Callback (CB : Event_Callback) is
   begin
      Callback := CB;
   end Set_Callback;

   ----------------
   -- Level_Name --
   ----------------

   function Level_Name (Level : Event_Level) return String is
   begin
      case Level is
         when Debug    => return "DEBUG";
         when Info     => return "INFO";
         when Warning  => return "WARN";
         when Error    => return "ERROR";
         when Critical => return "CRIT";
      end case;
   end Level_Name;

   ------------------
   -- Format_Event --
   ------------------

   function Format_Event (E : Event_Record) return String is
      ID_Str : constant String := Event_ID'Image (E.ID);
      TS_Str : constant String := Natural'Image (E.Timestamp);
   begin
      return "[" & Level_Name (E.Level) & "]" & TS_Str & " #" &
             ID_Str (ID_Str'First + 1 .. ID_Str'Last) & " " &
             E.Message (1 .. E.Msg_Len);
   end Format_Event;

end GNAT.Event_Log;
