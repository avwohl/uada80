-- GNAT.Callback body for Z80
-- Callback system implementation

package body GNAT.Callback is

   function Name_Match (R : Callback_Record; Name : String) return Boolean is
   begin
      if R.Name_Len /= Name'Length then
         return False;
      end if;
      for I in 1 .. R.Name_Len loop
         if R.Name (I) /= Name (Name'First + I - 1) then
            return False;
         end if;
      end loop;
      return True;
   end Name_Match;

   function Find_Callback (R : Callback_Registry; Name : String) return Natural is
   begin
      for I in 1 .. Max_Callbacks loop
         if R.Callbacks (I).Used and then Name_Match (R.Callbacks (I), Name) then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Callback;

   function Find_Free (R : Callback_Registry) return Natural is
   begin
      for I in 1 .. Max_Callbacks loop
         if not R.Callbacks (I).Used then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Free;

   procedure Store_Name (Rec : in Out Callback_Record; Name : String) is
      Len : Natural := Name'Length;
   begin
      if Len > Max_Name_Length then
         Len := Max_Name_Length;
      end if;
      Rec.Name := (others => ' ');
      Rec.Name_Len := Len;
      for I in 1 .. Len loop
         Rec.Name (I) := Name (Name'First + I - 1);
      end loop;
   end Store_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (R : out Callback_Registry) is
   begin
      R.Count := 0;
      for I in 1 .. Max_Callbacks loop
         R.Callbacks (I).Used := False;
         R.Callbacks (I).Name_Len := 0;
         R.Callbacks (I).CB_Type := None;
         R.Callbacks (I).Proc := null;
         R.Callbacks (I).Int_Proc := null;
         R.Callbacks (I).Bool_Proc := null;
         R.Callbacks (I).Func := null;
         R.Callbacks (I).Int_Func := null;
      end loop;
   end Initialize;

   --------------
   -- Register --
   --------------

   procedure Register (R : in Out Callback_Registry;
                       Name : String;
                       Proc : Callback_Proc) is
      Idx : Natural;
   begin
      -- Check if already registered
      Idx := Find_Callback (R, Name);
      if Idx = 0 then
         Idx := Find_Free (R);
         if Idx = 0 then
            return;  -- Full
         end if;
         R.Count := R.Count + 1;
      end if;

      Store_Name (R.Callbacks (Idx), Name);
      R.Callbacks (Idx).CB_Type := Simple;
      R.Callbacks (Idx).Proc := Proc;
      R.Callbacks (Idx).Used := True;
   end Register;

   procedure Register_Int (R : in Out Callback_Registry;
                           Name : String;
                           Proc : Callback_Int_Proc) is
      Idx : Natural;
   begin
      Idx := Find_Callback (R, Name);
      if Idx = 0 then
         Idx := Find_Free (R);
         if Idx = 0 then
            return;
         end if;
         R.Count := R.Count + 1;
      end if;

      Store_Name (R.Callbacks (Idx), Name);
      R.Callbacks (Idx).CB_Type := Int_Param;
      R.Callbacks (Idx).Int_Proc := Proc;
      R.Callbacks (Idx).Used := True;
   end Register_Int;

   procedure Register_Bool (R : in Out Callback_Registry;
                            Name : String;
                            Proc : Callback_Bool_Proc) is
      Idx : Natural;
   begin
      Idx := Find_Callback (R, Name);
      if Idx = 0 then
         Idx := Find_Free (R);
         if Idx = 0 then
            return;
         end if;
         R.Count := R.Count + 1;
      end if;

      Store_Name (R.Callbacks (Idx), Name);
      R.Callbacks (Idx).CB_Type := Bool_Param;
      R.Callbacks (Idx).Bool_Proc := Proc;
      R.Callbacks (Idx).Used := True;
   end Register_Bool;

   procedure Register_Func (R : in Out Callback_Registry;
                            Name : String;
                            Func : Callback_Func) is
      Idx : Natural;
   begin
      Idx := Find_Callback (R, Name);
      if Idx = 0 then
         Idx := Find_Free (R);
         if Idx = 0 then
            return;
         end if;
         R.Count := R.Count + 1;
      end if;

      Store_Name (R.Callbacks (Idx), Name);
      R.Callbacks (Idx).CB_Type := Returns_Bool;
      R.Callbacks (Idx).Func := Func;
      R.Callbacks (Idx).Used := True;
   end Register_Func;

   procedure Register_Int_Func (R : in Out Callback_Registry;
                                Name : String;
                                Func : Callback_Int_Func) is
      Idx : Natural;
   begin
      Idx := Find_Callback (R, Name);
      if Idx = 0 then
         Idx := Find_Free (R);
         if Idx = 0 then
            return;
         end if;
         R.Count := R.Count + 1;
      end if;

      Store_Name (R.Callbacks (Idx), Name);
      R.Callbacks (Idx).CB_Type := Returns_Int;
      R.Callbacks (Idx).Int_Func := Func;
      R.Callbacks (Idx).Used := True;
   end Register_Int_Func;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (R : in Out Callback_Registry; Name : String) is
      Idx : constant Natural := Find_Callback (R, Name);
   begin
      if Idx /= 0 then
         R.Callbacks (Idx).Used := False;
         R.Count := R.Count - 1;
      end if;
   end Unregister;

   -------------------
   -- Is_Registered --
   -------------------

   function Is_Registered (R : Callback_Registry; Name : String) return Boolean is
   begin
      return Find_Callback (R, Name) /= 0;
   end Is_Registered;

   ------------
   -- Invoke --
   ------------

   procedure Invoke (R : Callback_Registry; Name : String) is
      Idx : constant Natural := Find_Callback (R, Name);
   begin
      if Idx /= 0 and then
         R.Callbacks (Idx).CB_Type = Simple and then
         R.Callbacks (Idx).Proc /= null then
         R.Callbacks (Idx).Proc.all;
      end if;
   end Invoke;

   procedure Invoke_Int (R : Callback_Registry; Name : String; Value : Integer) is
      Idx : constant Natural := Find_Callback (R, Name);
   begin
      if Idx /= 0 and then
         R.Callbacks (Idx).CB_Type = Int_Param and then
         R.Callbacks (Idx).Int_Proc /= null then
         R.Callbacks (Idx).Int_Proc.all (Value);
      end if;
   end Invoke_Int;

   procedure Invoke_Bool (R : Callback_Registry; Name : String; Value : Boolean) is
      Idx : constant Natural := Find_Callback (R, Name);
   begin
      if Idx /= 0 and then
         R.Callbacks (Idx).CB_Type = Bool_Param and then
         R.Callbacks (Idx).Bool_Proc /= null then
         R.Callbacks (Idx).Bool_Proc.all (Value);
      end if;
   end Invoke_Bool;

   function Invoke_Func (R : Callback_Registry; Name : String) return Boolean is
      Idx : constant Natural := Find_Callback (R, Name);
   begin
      if Idx /= 0 and then
         R.Callbacks (Idx).CB_Type = Returns_Bool and then
         R.Callbacks (Idx).Func /= null then
         return R.Callbacks (Idx).Func.all;
      end if;
      return False;
   end Invoke_Func;

   function Invoke_Int_Func (R : Callback_Registry; Name : String;
                             Value : Integer) return Integer is
      Idx : constant Natural := Find_Callback (R, Name);
   begin
      if Idx /= 0 and then
         R.Callbacks (Idx).CB_Type = Returns_Int and then
         R.Callbacks (Idx).Int_Func /= null then
         return R.Callbacks (Idx).Int_Func.all (Value);
      end if;
      return 0;
   end Invoke_Int_Func;

   -----------
   -- Count --
   -----------

   function Count (R : Callback_Registry) return Natural is
   begin
      return R.Count;
   end Count;

   -----------
   -- Clear --
   -----------

   procedure Clear (R : out Callback_Registry) is
   begin
      Initialize (R);
   end Clear;

   ------------------
   -- Event System --
   ------------------

   procedure Init_Events (E : out Event_System) is
   begin
      for I in 0 .. Max_Event_Types - 1 loop
         E.Events (I).Count := 0;
         for J in 1 .. Max_Event_Handlers loop
            E.Events (I).Handlers (J) := null;
         end loop;
      end loop;
   end Init_Events;

   procedure Subscribe (E : in Out Event_System;
                        Event_Type : Natural;
                        Handler : Callback_Int_Proc) is
   begin
      if Event_Type >= Max_Event_Types or Handler = null then
         return;
      end if;

      -- Check if already subscribed
      for I in 1 .. E.Events (Event_Type).Count loop
         if E.Events (Event_Type).Handlers (I) = Handler then
            return;
         end if;
      end loop;

      -- Add handler
      if E.Events (Event_Type).Count < Max_Event_Handlers then
         E.Events (Event_Type).Count := E.Events (Event_Type).Count + 1;
         E.Events (Event_Type).Handlers (E.Events (Event_Type).Count) := Handler;
      end if;
   end Subscribe;

   procedure Unsubscribe (E : in Out Event_System;
                          Event_Type : Natural;
                          Handler : Callback_Int_Proc) is
   begin
      if Event_Type >= Max_Event_Types then
         return;
      end if;

      for I in 1 .. E.Events (Event_Type).Count loop
         if E.Events (Event_Type).Handlers (I) = Handler then
            -- Shift remaining handlers
            for J in I .. E.Events (Event_Type).Count - 1 loop
               E.Events (Event_Type).Handlers (J) :=
                 E.Events (Event_Type).Handlers (J + 1);
            end loop;
            E.Events (Event_Type).Count := E.Events (Event_Type).Count - 1;
            return;
         end if;
      end loop;
   end Unsubscribe;

   procedure Fire_Event (E : Event_System;
                         Event_Type : Natural;
                         Data : Integer := 0) is
   begin
      if Event_Type >= Max_Event_Types then
         return;
      end if;

      for I in 1 .. E.Events (Event_Type).Count loop
         if E.Events (Event_Type).Handlers (I) /= null then
            E.Events (Event_Type).Handlers (I).all (Data);
         end if;
      end loop;
   end Fire_Event;

   function Handler_Count (E : Event_System; Event_Type : Natural) return Natural is
   begin
      if Event_Type >= Max_Event_Types then
         return 0;
      end if;
      return E.Events (Event_Type).Count;
   end Handler_Count;

   procedure Clear_Event_Type (E : in Out Event_System; Event_Type : Natural) is
   begin
      if Event_Type < Max_Event_Types then
         E.Events (Event_Type).Count := 0;
      end if;
   end Clear_Event_Type;

   procedure Clear_All_Events (E : out Event_System) is
   begin
      Init_Events (E);
   end Clear_All_Events;

   -----------------
   -- Hook System --
   -----------------

   procedure Init_Hooks (H : out Hook_System) is
   begin
      for I in 0 .. Max_Hooks - 1 loop
         H.Hooks (I) := null;
      end loop;
   end Init_Hooks;

   procedure Set_Hook (H : in Out Hook_System;
                       Hook_Id : Natural;
                       Proc : Callback_Proc) is
   begin
      if Hook_Id < Max_Hooks then
         H.Hooks (Hook_Id) := Proc;
      end if;
   end Set_Hook;

   procedure Clear_Hook (H : in Out Hook_System; Hook_Id : Natural) is
   begin
      if Hook_Id < Max_Hooks then
         H.Hooks (Hook_Id) := null;
      end if;
   end Clear_Hook;

   function Has_Hook (H : Hook_System; Hook_Id : Natural) return Boolean is
   begin
      if Hook_Id >= Max_Hooks then
         return False;
      end if;
      return H.Hooks (Hook_Id) /= null;
   end Has_Hook;

   procedure Call_Hook (H : Hook_System; Hook_Id : Natural) is
   begin
      if Hook_Id < Max_Hooks and then H.Hooks (Hook_Id) /= null then
         H.Hooks (Hook_Id).all;
      end if;
   end Call_Hook;

   procedure Clear_All_Hooks (H : out Hook_System) is
   begin
      Init_Hooks (H);
   end Clear_All_Hooks;

   --------------------
   -- Callback Chain --
   --------------------

   procedure Init_Chain (C : out Callback_Chain) is
   begin
      C.Length := 0;
      for I in 1 .. Max_Chain_Length loop
         C.Procs (I) := null;
      end loop;
   end Init_Chain;

   procedure Add_To_Chain (C : in Out Callback_Chain; Proc : Callback_Proc) is
   begin
      if C.Length < Max_Chain_Length and Proc /= null then
         C.Length := C.Length + 1;
         C.Procs (C.Length) := Proc;
      end if;
   end Add_To_Chain;

   procedure Remove_From_Chain (C : in Out Callback_Chain; Proc : Callback_Proc) is
   begin
      for I in 1 .. C.Length loop
         if C.Procs (I) = Proc then
            for J in I .. C.Length - 1 loop
               C.Procs (J) := C.Procs (J + 1);
            end loop;
            C.Length := C.Length - 1;
            return;
         end if;
      end loop;
   end Remove_From_Chain;

   procedure Execute_Chain (C : Callback_Chain) is
   begin
      for I in 1 .. C.Length loop
         if C.Procs (I) /= null then
            C.Procs (I).all;
         end if;
      end loop;
   end Execute_Chain;

   function Chain_Length (C : Callback_Chain) return Natural is
   begin
      return C.Length;
   end Chain_Length;

   procedure Clear_Chain (C : out Callback_Chain) is
   begin
      Init_Chain (C);
   end Clear_Chain;

end GNAT.Callback;
