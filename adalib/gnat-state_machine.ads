-- GNAT.State_Machine for Z80
-- Simple finite state machine framework

package GNAT.State_Machine is
   pragma Preelaborate;

   Max_States : constant := 16;  -- Maximum states for Z80
   Max_Events : constant := 16;  -- Maximum event types
   Max_Transitions : constant := 64;  -- Maximum transitions

   type State_Type is range 0 .. Max_States - 1;
   type Event_Type is range 0 .. Max_Events - 1;

   Invalid_State : constant State_Type := 0;
   Initial_State : constant State_Type := 1;

   type Transition_Action is access procedure;
   type Guard_Condition is access function return Boolean;

   type Machine is private;

   procedure Initialize (M : out Machine; Initial : State_Type := Initial_State);
   --  Initialize state machine with initial state

   procedure Add_Transition
     (M         : in Out Machine;
      From      : State_Type;
      Event     : Event_Type;
      To        : State_Type;
      Action    : Transition_Action := null;
      Guard     : Guard_Condition := null);
   --  Add a transition rule

   procedure Process_Event (M : in Out Machine; Event : Event_Type);
   --  Process an event, potentially triggering state change

   function Current_State (M : Machine) return State_Type;
   --  Return current state

   function Previous_State (M : Machine) return State_Type;
   --  Return previous state

   function Last_Event (M : Machine) return Event_Type;
   --  Return last processed event

   function Transition_Count (M : Machine) return Natural;
   --  Return number of state transitions

   procedure Reset (M : in Out Machine);
   --  Reset to initial state

   function Can_Accept (M : Machine; Event : Event_Type) return Boolean;
   --  Check if event can be accepted from current state

   -- Entry/Exit actions
   type State_Action is access procedure (S : State_Type);

   procedure Set_Entry_Action (M : in Out Machine; S : State_Type;
                               Action : State_Action);
   --  Set action to run when entering state

   procedure Set_Exit_Action (M : in Out Machine; S : State_Type;
                              Action : State_Action);
   --  Set action to run when exiting state

private

   type Transition is record
      From      : State_Type := Invalid_State;
      Event     : Event_Type := 0;
      To        : State_Type := Invalid_State;
      Action    : Transition_Action := null;
      Guard     : Guard_Condition := null;
      Valid     : Boolean := False;
   end record;

   type Transition_Array is array (1 .. Max_Transitions) of Transition;
   type State_Action_Array is array (State_Type) of State_Action;

   type Machine is record
      Transitions     : Transition_Array;
      Trans_Count     : Natural := 0;
      Entry_Actions   : State_Action_Array := (others => null);
      Exit_Actions    : State_Action_Array := (others => null);
      Current         : State_Type := Initial_State;
      Previous        : State_Type := Invalid_State;
      Last_Evt        : Event_Type := 0;
      Initial         : State_Type := Initial_State;
      Trans_Total     : Natural := 0;  -- Total transitions made
   end record;

end GNAT.State_Machine;
