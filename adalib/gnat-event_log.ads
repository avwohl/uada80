-- GNAT.Event_Log for Z80
-- Simple event logging with bounded storage

package GNAT.Event_Log is
   pragma Preelaborate;

   Max_Events     : constant := 32;   -- Maximum logged events
   Max_Msg_Length : constant := 24;   -- Maximum message length

   type Event_Level is (Debug, Info, Warning, Error, Critical);

   type Event_ID is new Natural range 0 .. 255;

   -- Initialize the event log
   procedure Initialize;

   -- Log events at various levels
   procedure Log_Debug (ID : Event_ID; Msg : String := "");
   procedure Log_Info (ID : Event_ID; Msg : String := "");
   procedure Log_Warning (ID : Event_ID; Msg : String := "");
   procedure Log_Error (ID : Event_ID; Msg : String := "");
   procedure Log_Critical (ID : Event_ID; Msg : String := "");

   -- Generic log with explicit level
   procedure Log (Level : Event_Level; ID : Event_ID; Msg : String := "");

   -- Log with numeric value
   procedure Log_Value (Level : Event_Level; ID : Event_ID; Value : Integer);

   -- Set minimum level for logging (events below this are ignored)
   procedure Set_Min_Level (Level : Event_Level);
   function Get_Min_Level return Event_Level;

   -- Get event counts
   function Event_Count return Natural;
   function Event_Count (Level : Event_Level) return Natural;

   -- Clear all events
   procedure Clear;

   -- Check if log is full
   function Is_Full return Boolean;

   -- Check if events have been dropped (log overflow)
   function Has_Overflow return Boolean;
   procedure Clear_Overflow_Flag;

   -- Event record for retrieval
   type Event_Record is record
      Level     : Event_Level;
      ID        : Event_ID;
      Timestamp : Natural;  -- Tick count when logged
      Message   : String (1 .. Max_Msg_Length);
      Msg_Len   : Natural;
   end record;

   -- Retrieve events (oldest first)
   function Get_Event (Index : Positive) return Event_Record;

   -- Get most recent event
   function Get_Latest return Event_Record;

   -- Get oldest event
   function Get_Oldest return Event_Record;

   -- Find events by criteria
   function Find_By_Level (Level : Event_Level; Start : Positive := 1) return Natural;
   function Find_By_ID (ID : Event_ID; Start : Positive := 1) return Natural;

   -- Callback type for event notification
   type Event_Callback is access procedure (E : Event_Record);

   -- Set callback for immediate notification (null to disable)
   procedure Set_Callback (CB : Event_Callback);

   -- Level name as string
   function Level_Name (Level : Event_Level) return String;

   -- Format event as string (for output)
   function Format_Event (E : Event_Record) return String;

private

   type Event_Array is array (1 .. Max_Events) of Event_Record;

   Events       : Event_Array;
   Count        : Natural := 0;
   Write_Index  : Natural := 0;
   Read_Index   : Natural := 1;
   Min_Level    : Event_Level := Debug;
   Overflow     : Boolean := False;
   Callback     : Event_Callback := null;

   Level_Counts : array (Event_Level) of Natural := (others => 0);

end GNAT.Event_Log;
