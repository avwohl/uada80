-- GNAT.State_Machine body for Z80
-- Simple finite state machine implementation

package body GNAT.State_Machine is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (M : out Machine; Initial : State_Type := Initial_State) is
   begin
      M.Transitions := (others => (From   => Invalid_State,
                                   Event  => 0,
                                   To     => Invalid_State,
                                   Action => null,
                                   Guard  => null,
                                   Valid  => False));
      M.Trans_Count := 0;
      M.Entry_Actions := (others => null);
      M.Exit_Actions := (others => null);
      M.Current := Initial;
      M.Previous := Invalid_State;
      M.Last_Evt := 0;
      M.Initial := Initial;
      M.Trans_Total := 0;
   end Initialize;

   --------------------
   -- Add_Transition --
   --------------------

   procedure Add_Transition
     (M         : in Out Machine;
      From      : State_Type;
      Event     : Event_Type;
      To        : State_Type;
      Action    : Transition_Action := null;
      Guard     : Guard_Condition := null)
   is
   begin
      if M.Trans_Count >= Max_Transitions then
         return;  -- Table full
      end if;

      M.Trans_Count := M.Trans_Count + 1;
      M.Transitions (M.Trans_Count) :=
        (From   => From,
         Event  => Event,
         To     => To,
         Action => Action,
         Guard  => Guard,
         Valid  => True);
   end Add_Transition;

   -------------------
   -- Process_Event --
   -------------------

   procedure Process_Event (M : in Out Machine; Event : Event_Type) is
   begin
      M.Last_Evt := Event;

      -- Find matching transition
      for I in 1 .. M.Trans_Count loop
         declare
            T : Transition renames M.Transitions (I);
         begin
            if T.Valid and then T.From = M.Current and then T.Event = Event then
               -- Check guard condition
               if T.Guard = null or else T.Guard.all then
                  -- Execute exit action
                  if M.Exit_Actions (M.Current) /= null then
                     M.Exit_Actions (M.Current) (M.Current);
                  end if;

                  -- Execute transition action
                  if T.Action /= null then
                     T.Action.all;
                  end if;

                  -- Update state
                  M.Previous := M.Current;
                  M.Current := T.To;
                  M.Trans_Total := M.Trans_Total + 1;

                  -- Execute entry action
                  if M.Entry_Actions (M.Current) /= null then
                     M.Entry_Actions (M.Current) (M.Current);
                  end if;

                  return;
               end if;
            end if;
         end;
      end loop;

      -- No matching transition found - event ignored
   end Process_Event;

   -------------------
   -- Current_State --
   -------------------

   function Current_State (M : Machine) return State_Type is
   begin
      return M.Current;
   end Current_State;

   --------------------
   -- Previous_State --
   --------------------

   function Previous_State (M : Machine) return State_Type is
   begin
      return M.Previous;
   end Previous_State;

   ----------------
   -- Last_Event --
   ----------------

   function Last_Event (M : Machine) return Event_Type is
   begin
      return M.Last_Evt;
   end Last_Event;

   ----------------------
   -- Transition_Count --
   ----------------------

   function Transition_Count (M : Machine) return Natural is
   begin
      return M.Trans_Total;
   end Transition_Count;

   -----------
   -- Reset --
   -----------

   procedure Reset (M : in Out Machine) is
   begin
      if M.Current /= M.Initial then
         -- Execute exit action for current state
         if M.Exit_Actions (M.Current) /= null then
            M.Exit_Actions (M.Current) (M.Current);
         end if;

         M.Previous := M.Current;
         M.Current := M.Initial;

         -- Execute entry action for initial state
         if M.Entry_Actions (M.Current) /= null then
            M.Entry_Actions (M.Current) (M.Current);
         end if;
      end if;
      M.Trans_Total := 0;
   end Reset;

   ----------------
   -- Can_Accept --
   ----------------

   function Can_Accept (M : Machine; Event : Event_Type) return Boolean is
   begin
      for I in 1 .. M.Trans_Count loop
         declare
            T : Transition renames M.Transitions (I);
         begin
            if T.Valid and then T.From = M.Current and then T.Event = Event then
               if T.Guard = null or else T.Guard.all then
                  return True;
               end if;
            end if;
         end;
      end loop;
      return False;
   end Can_Accept;

   ----------------------
   -- Set_Entry_Action --
   ----------------------

   procedure Set_Entry_Action (M : in Out Machine; S : State_Type;
                               Action : State_Action)
   is
   begin
      M.Entry_Actions (S) := Action;
   end Set_Entry_Action;

   ---------------------
   -- Set_Exit_Action --
   ---------------------

   procedure Set_Exit_Action (M : in Out Machine; S : State_Type;
                              Action : State_Action)
   is
   begin
      M.Exit_Actions (S) := Action;
   end Set_Exit_Action;

end GNAT.State_Machine;
