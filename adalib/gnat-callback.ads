-- GNAT.Callback for Z80
-- Callback registration and invocation system

package GNAT.Callback is
   pragma Preelaborate;

   Max_Callbacks : constant := 16;

   -- Callback procedure types
   type Callback_Proc is access procedure;
   type Callback_Int_Proc is access procedure (Value : Integer);
   type Callback_Bool_Proc is access procedure (Value : Boolean);

   -- Callback function types
   type Callback_Func is access function return Boolean;
   type Callback_Int_Func is access function (Value : Integer) return Integer;

   -- Callback registry
   type Callback_Registry is limited private;

   -- Initialize registry
   procedure Initialize (R : out Callback_Registry);

   -- Register callbacks by name (up to 8 characters)
   procedure Register (R : in Out Callback_Registry;
                       Name : String;
                       Proc : Callback_Proc);
   procedure Register_Int (R : in Out Callback_Registry;
                           Name : String;
                           Proc : Callback_Int_Proc);
   procedure Register_Bool (R : in Out Callback_Registry;
                            Name : String;
                            Proc : Callback_Bool_Proc);
   procedure Register_Func (R : in Out Callback_Registry;
                            Name : String;
                            Func : Callback_Func);
   procedure Register_Int_Func (R : in Out Callback_Registry;
                                Name : String;
                                Func : Callback_Int_Func);

   -- Unregister
   procedure Unregister (R : in Out Callback_Registry; Name : String);

   -- Check if registered
   function Is_Registered (R : Callback_Registry; Name : String) return Boolean;

   -- Invoke callbacks
   procedure Invoke (R : Callback_Registry; Name : String);
   procedure Invoke_Int (R : Callback_Registry; Name : String; Value : Integer);
   procedure Invoke_Bool (R : Callback_Registry; Name : String; Value : Boolean);
   function Invoke_Func (R : Callback_Registry; Name : String) return Boolean;
   function Invoke_Int_Func (R : Callback_Registry; Name : String;
                             Value : Integer) return Integer;

   -- Count registered callbacks
   function Count (R : Callback_Registry) return Natural;

   -- Clear all callbacks
   procedure Clear (R : out Callback_Registry);

   -- Simple event system
   type Event_System is limited private;
   Max_Event_Handlers : constant := 8;
   Max_Event_Types : constant := 8;

   procedure Init_Events (E : out Event_System);
   procedure Subscribe (E : in Out Event_System;
                        Event_Type : Natural;
                        Handler : Callback_Int_Proc);
   procedure Unsubscribe (E : in Out Event_System;
                          Event_Type : Natural;
                          Handler : Callback_Int_Proc);
   procedure Fire_Event (E : Event_System;
                         Event_Type : Natural;
                         Data : Integer := 0);
   function Handler_Count (E : Event_System; Event_Type : Natural) return Natural;
   procedure Clear_Event_Type (E : in Out Event_System; Event_Type : Natural);
   procedure Clear_All_Events (E : out Event_System);

   -- Hook system (single callback per hook point)
   type Hook_System is limited private;
   Max_Hooks : constant := 16;

   procedure Init_Hooks (H : out Hook_System);
   procedure Set_Hook (H : in Out Hook_System;
                       Hook_Id : Natural;
                       Proc : Callback_Proc);
   procedure Clear_Hook (H : in Out Hook_System; Hook_Id : Natural);
   function Has_Hook (H : Hook_System; Hook_Id : Natural) return Boolean;
   procedure Call_Hook (H : Hook_System; Hook_Id : Natural);
   procedure Clear_All_Hooks (H : out Hook_System);

   -- Chain callbacks (call in sequence)
   type Callback_Chain is limited private;
   Max_Chain_Length : constant := 8;

   procedure Init_Chain (C : out Callback_Chain);
   procedure Add_To_Chain (C : in Out Callback_Chain; Proc : Callback_Proc);
   procedure Remove_From_Chain (C : in Out Callback_Chain; Proc : Callback_Proc);
   procedure Execute_Chain (C : Callback_Chain);
   function Chain_Length (C : Callback_Chain) return Natural;
   procedure Clear_Chain (C : out Callback_Chain);

private

   Max_Name_Length : constant := 8;

   type Callback_Type is (None, Simple, Int_Param, Bool_Param, Returns_Bool, Returns_Int);

   type Callback_Record is record
      Name     : String (1 .. Max_Name_Length);
      Name_Len : Natural;
      CB_Type  : Callback_Type;
      Proc     : Callback_Proc;
      Int_Proc : Callback_Int_Proc;
      Bool_Proc : Callback_Bool_Proc;
      Func     : Callback_Func;
      Int_Func : Callback_Int_Func;
      Used     : Boolean;
   end record;

   type Callback_Array is array (1 .. Max_Callbacks) of Callback_Record;

   type Callback_Registry is limited record
      Callbacks : Callback_Array;
      Count     : Natural := 0;
   end record;

   type Handler_Array is array (1 .. Max_Event_Handlers) of Callback_Int_Proc;
   type Event_Record is record
      Handlers : Handler_Array;
      Count    : Natural;
   end record;
   type Event_Array is array (0 .. Max_Event_Types - 1) of Event_Record;

   type Event_System is limited record
      Events : Event_Array;
   end record;

   type Hook_Array is array (0 .. Max_Hooks - 1) of Callback_Proc;

   type Hook_System is limited record
      Hooks : Hook_Array;
   end record;

   type Chain_Array is array (1 .. Max_Chain_Length) of Callback_Proc;

   type Callback_Chain is limited record
      Procs  : Chain_Array;
      Length : Natural := 0;
   end record;

end GNAT.Callback;
