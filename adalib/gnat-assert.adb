-- GNAT.Assert body for Z80
-- Assertion implementation

package body GNAT.Assert is

   procedure Record_Failure (Message : String) is
   begin
      Failures := Failures + 1;

      -- Store message
      Last_Message_Len := Message'Length;
      if Last_Message_Len > Last_Message'Length then
         Last_Message_Len := Last_Message'Length;
      end if;
      Last_Message := (others => ' ');
      Last_Message (1 .. Last_Message_Len) :=
        Message (Message'First .. Message'First + Last_Message_Len - 1);

      case Current_Mode is
         when Halt_On_Failure =>
            raise Constraint_Error with Message;
         when Continue_On_Failure | Log_Only =>
            null;  -- Continue execution
      end case;
   end Record_Failure;

   procedure Record_Pass is
   begin
      Passes := Passes + 1;
   end Record_Pass;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (Mode : Failure_Mode) is
   begin
      Current_Mode := Mode;
   end Set_Mode;

   --------------
   -- Get_Mode --
   --------------

   function Get_Mode return Failure_Mode is
   begin
      return Current_Mode;
   end Get_Mode;

   ------------
   -- Assert --
   ------------

   procedure Assert (Condition : Boolean; Message : String := "") is
   begin
      if Condition then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure ("Assertion failed: " & Message);
         else
            Record_Failure ("Assertion failed");
         end if;
      end if;
   end Assert;

   -----------------
   -- Assert_True --
   -----------------

   procedure Assert_True (Condition : Boolean; Message : String := "") is
   begin
      if Condition then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure ("Expected TRUE: " & Message);
         else
            Record_Failure ("Expected TRUE, got FALSE");
         end if;
      end if;
   end Assert_True;

   ------------------
   -- Assert_False --
   ------------------

   procedure Assert_False (Condition : Boolean; Message : String := "") is
   begin
      if not Condition then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure ("Expected FALSE: " & Message);
         else
            Record_Failure ("Expected FALSE, got TRUE");
         end if;
      end if;
   end Assert_False;

   ------------------
   -- Assert_Equal --
   ------------------

   procedure Assert_Equal (Left, Right : Integer; Message : String := "") is
   begin
      if Left = Right then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("Expected equal integers");
         end if;
      end if;
   end Assert_Equal;

   procedure Assert_Equal (Left, Right : String; Message : String := "") is
   begin
      if Left = Right then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("Expected equal strings");
         end if;
      end if;
   end Assert_Equal;

   procedure Assert_Equal (Left, Right : Character; Message : String := "") is
   begin
      if Left = Right then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("Expected equal characters");
         end if;
      end if;
   end Assert_Equal;

   procedure Assert_Equal (Left, Right : Boolean; Message : String := "") is
   begin
      if Left = Right then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("Expected equal booleans");
         end if;
      end if;
   end Assert_Equal;

   ----------------------
   -- Assert_Not_Equal --
   ----------------------

   procedure Assert_Not_Equal (Left, Right : Integer; Message : String := "") is
   begin
      if Left /= Right then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("Expected different integers");
         end if;
      end if;
   end Assert_Not_Equal;

   procedure Assert_Not_Equal (Left, Right : String; Message : String := "") is
   begin
      if Left /= Right then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("Expected different strings");
         end if;
      end if;
   end Assert_Not_Equal;

   --------------------
   -- Assert_Greater --
   --------------------

   procedure Assert_Greater (Left, Right : Integer; Message : String := "") is
   begin
      if Left > Right then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("Expected Left > Right");
         end if;
      end if;
   end Assert_Greater;

   -----------------
   -- Assert_Less --
   -----------------

   procedure Assert_Less (Left, Right : Integer; Message : String := "") is
   begin
      if Left < Right then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("Expected Left < Right");
         end if;
      end if;
   end Assert_Less;

   --------------------------
   -- Assert_Greater_Equal --
   --------------------------

   procedure Assert_Greater_Equal (Left, Right : Integer; Message : String := "") is
   begin
      if Left >= Right then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("Expected Left >= Right");
         end if;
      end if;
   end Assert_Greater_Equal;

   -----------------------
   -- Assert_Less_Equal --
   -----------------------

   procedure Assert_Less_Equal (Left, Right : Integer; Message : String := "") is
   begin
      if Left <= Right then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("Expected Left <= Right");
         end if;
      end if;
   end Assert_Less_Equal;

   ---------------------
   -- Assert_In_Range --
   ---------------------

   procedure Assert_In_Range (Value, Low, High : Integer; Message : String := "") is
   begin
      if Value >= Low and Value <= High then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("Value not in range");
         end if;
      end if;
   end Assert_In_Range;

   -------------------------
   -- Assert_Not_In_Range --
   -------------------------

   procedure Assert_Not_In_Range (Value, Low, High : Integer; Message : String := "") is
   begin
      if Value < Low or Value > High then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("Value unexpectedly in range");
         end if;
      end if;
   end Assert_Not_In_Range;

   ----------------------
   -- Assert_Not_Empty --
   ----------------------

   procedure Assert_Not_Empty (S : String; Message : String := "") is
   begin
      if S'Length > 0 then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("Expected non-empty string");
         end if;
      end if;
   end Assert_Not_Empty;

   -------------------
   -- Assert_Length --
   -------------------

   procedure Assert_Length (S : String; Expected : Natural; Message : String := "") is
   begin
      if S'Length = Expected then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("String length mismatch");
         end if;
      end if;
   end Assert_Length;

   ---------------------
   -- Assert_Contains --
   ---------------------

   procedure Assert_Contains (S, Substring : String; Message : String := "") is
      Found : Boolean := False;
   begin
      if Substring'Length <= S'Length then
         for I in S'First .. S'Last - Substring'Length + 1 loop
            if S (I .. I + Substring'Length - 1) = Substring then
               Found := True;
               exit;
            end if;
         end loop;
      end if;

      if Found then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("Substring not found");
         end if;
      end if;
   end Assert_Contains;

   ------------------------
   -- Assert_Starts_With --
   ------------------------

   procedure Assert_Starts_With (S, Prefix : String; Message : String := "") is
   begin
      if S'Length >= Prefix'Length and then
         S (S'First .. S'First + Prefix'Length - 1) = Prefix then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("String doesn't start with prefix");
         end if;
      end if;
   end Assert_Starts_With;

   ----------------------
   -- Assert_Ends_With --
   ----------------------

   procedure Assert_Ends_With (S, Suffix : String; Message : String := "") is
   begin
      if S'Length >= Suffix'Length and then
         S (S'Last - Suffix'Length + 1 .. S'Last) = Suffix then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("String doesn't end with suffix");
         end if;
      end if;
   end Assert_Ends_With;

   ---------------------
   -- Assert_Not_Null --
   ---------------------

   procedure Assert_Not_Null (Value : Integer; Message : String := "") is
   begin
      if Value /= 0 then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("Expected non-zero value");
         end if;
      end if;
   end Assert_Not_Null;

   ---------------------
   -- Assert_Positive --
   ---------------------

   procedure Assert_Positive (Value : Integer; Message : String := "") is
   begin
      if Value > 0 then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("Expected positive value");
         end if;
      end if;
   end Assert_Positive;

   -------------------------
   -- Assert_Non_Negative --
   -------------------------

   procedure Assert_Non_Negative (Value : Integer; Message : String := "") is
   begin
      if Value >= 0 then
         Record_Pass;
      else
         if Message'Length > 0 then
            Record_Failure (Message);
         else
            Record_Failure ("Expected non-negative value");
         end if;
      end if;
   end Assert_Non_Negative;

   -------------------
   -- Failure_Count --
   -------------------

   function Failure_Count return Natural is
   begin
      return Failures;
   end Failure_Count;

   ----------------
   -- Pass_Count --
   ----------------

   function Pass_Count return Natural is
   begin
      return Passes;
   end Pass_Count;

   -----------------
   -- Total_Count --
   -----------------

   function Total_Count return Natural is
   begin
      return Passes + Failures;
   end Total_Count;

   ------------------
   -- Reset_Counts --
   ------------------

   procedure Reset_Counts is
   begin
      Passes := 0;
      Failures := 0;
      Last_Message_Len := 0;
   end Reset_Counts;

   --------------------------
   -- Last_Failure_Message --
   --------------------------

   function Last_Failure_Message return String is
   begin
      return Last_Message (1 .. Last_Message_Len);
   end Last_Failure_Message;

   -------------------
   -- Print_Summary --
   -------------------

   procedure Print_Summary is
   begin
      -- Would output to console in actual implementation
      null;
   end Print_Summary;

   ----------
   -- Fail --
   ----------

   procedure Fail (Message : String := "") is
   begin
      if Message'Length > 0 then
         Record_Failure (Message);
      else
         Record_Failure ("Explicit failure");
      end if;
   end Fail;

   -----------------
   -- Unreachable --
   -----------------

   procedure Unreachable (Message : String := "Unreachable code executed") is
   begin
      Record_Failure (Message);
   end Unreachable;

   -----------------
   -- Debug_Print --
   -----------------

   procedure Debug_Print (Message : String) is
   begin
      if Debug_Active then
         -- Would output to console
         null;
      end if;
   end Debug_Print;

   -----------------
   -- Debug_Value --
   -----------------

   procedure Debug_Value (Name : String; Value : Integer) is
      pragma Unreferenced (Name, Value);
   begin
      if Debug_Active then
         null;
      end if;
   end Debug_Value;

   procedure Debug_Value (Name : String; Value : String) is
      pragma Unreferenced (Name, Value);
   begin
      if Debug_Active then
         null;
      end if;
   end Debug_Value;

   procedure Debug_Value (Name : String; Value : Boolean) is
      pragma Unreferenced (Name, Value);
   begin
      if Debug_Active then
         null;
      end if;
   end Debug_Value;

   ------------------
   -- Enable_Debug --
   ------------------

   procedure Enable_Debug is
   begin
      Debug_Active := True;
   end Enable_Debug;

   -------------------
   -- Disable_Debug --
   -------------------

   procedure Disable_Debug is
   begin
      Debug_Active := False;
   end Disable_Debug;

   -------------------
   -- Debug_Enabled --
   -------------------

   function Debug_Enabled return Boolean is
   begin
      return Debug_Active;
   end Debug_Enabled;

end GNAT.Assert;
