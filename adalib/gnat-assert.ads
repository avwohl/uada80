-- GNAT.Assert for Z80
-- Simple assertion and debugging utilities

package GNAT.Assert is
   pragma Preelaborate;

   -- Assertion failure handling modes
   type Failure_Mode is (Halt_On_Failure, Continue_On_Failure, Log_Only);

   -- Set global failure mode
   procedure Set_Mode (Mode : Failure_Mode);
   function Get_Mode return Failure_Mode;

   -- Basic assertions
   procedure Assert (Condition : Boolean; Message : String := "");
   procedure Assert_True (Condition : Boolean; Message : String := "");
   procedure Assert_False (Condition : Boolean; Message : String := "");

   -- Equality assertions
   procedure Assert_Equal (Left, Right : Integer; Message : String := "");
   procedure Assert_Equal (Left, Right : String; Message : String := "");
   procedure Assert_Equal (Left, Right : Character; Message : String := "");
   procedure Assert_Equal (Left, Right : Boolean; Message : String := "");

   -- Inequality assertions
   procedure Assert_Not_Equal (Left, Right : Integer; Message : String := "");
   procedure Assert_Not_Equal (Left, Right : String; Message : String := "");

   -- Comparison assertions
   procedure Assert_Greater (Left, Right : Integer; Message : String := "");
   procedure Assert_Less (Left, Right : Integer; Message : String := "");
   procedure Assert_Greater_Equal (Left, Right : Integer; Message : String := "");
   procedure Assert_Less_Equal (Left, Right : Integer; Message : String := "");

   -- Range assertions
   procedure Assert_In_Range (Value, Low, High : Integer; Message : String := "");
   procedure Assert_Not_In_Range (Value, Low, High : Integer; Message : String := "");

   -- String assertions
   procedure Assert_Not_Empty (S : String; Message : String := "");
   procedure Assert_Length (S : String; Expected : Natural; Message : String := "");
   procedure Assert_Contains (S, Substring : String; Message : String := "");
   procedure Assert_Starts_With (S, Prefix : String; Message : String := "");
   procedure Assert_Ends_With (S, Suffix : String; Message : String := "");

   -- Null/validity assertions
   procedure Assert_Not_Null (Value : Integer; Message : String := "");
   procedure Assert_Positive (Value : Integer; Message : String := "");
   procedure Assert_Non_Negative (Value : Integer; Message : String := "");

   -- Failure tracking
   function Failure_Count return Natural;
   function Pass_Count return Natural;
   function Total_Count return Natural;
   procedure Reset_Counts;

   -- Get last failure message
   function Last_Failure_Message return String;

   -- Summary output
   procedure Print_Summary;

   -- Fail unconditionally
   procedure Fail (Message : String := "");

   -- Unreachable code marker
   procedure Unreachable (Message : String := "Unreachable code executed");

   -- Debug output (only if assertions enabled)
   procedure Debug_Print (Message : String);
   procedure Debug_Value (Name : String; Value : Integer);
   procedure Debug_Value (Name : String; Value : String);
   procedure Debug_Value (Name : String; Value : Boolean);

   -- Enable/disable debug output
   procedure Enable_Debug;
   procedure Disable_Debug;
   function Debug_Enabled return Boolean;

private

   Current_Mode     : Failure_Mode := Halt_On_Failure;
   Failures         : Natural := 0;
   Passes           : Natural := 0;
   Debug_Active     : Boolean := False;
   Last_Message     : String (1 .. 64) := (others => ' ');
   Last_Message_Len : Natural := 0;

end GNAT.Assert;
