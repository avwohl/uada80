-- GNAT.Sockets.Poll body for Z80
-- Socket polling stub implementation

package body GNAT.Sockets.Poll is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Set : out Poll_Set) is
   begin
      Set.Count := 0;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Set : in Out Poll_Set) is
   begin
      Set.Count := 0;
   end Finalize;

   ----------------
   -- Add_Socket --
   ----------------

   procedure Add_Socket
     (Set    : in Out Poll_Set;
      Socket : Socket_Type;
      Events : Event_Set)
   is
   begin
      if Set.Count < Max_Sockets then
         Set.Count := Set.Count + 1;
         Set.Sockets (Set.Count) := (Socket => Socket, Events => Events, Active => True);
      end if;
   end Add_Socket;

   -------------------
   -- Remove_Socket --
   -------------------

   procedure Remove_Socket
     (Set    : in Out Poll_Set;
      Socket : Socket_Type)
   is
   begin
      for I in 1 .. Set.Count loop
         if Set.Sockets (I).Socket = Socket then
            Set.Sockets (I).Active := False;
            exit;
         end if;
      end loop;
   end Remove_Socket;

   ----------
   -- Wait --
   ----------

   procedure Wait
     (Set     : Poll_Set;
      Timeout : Duration;
      Count   : out Natural)
   is
      pragma Unreferenced (Set, Timeout);
   begin
      -- No actual polling on CP/M - always returns 0
      Count := 0;
   end Wait;

   ------------
   -- Events --
   ------------

   function Events
     (Set    : Poll_Set;
      Socket : Socket_Type) return Event_Set
   is
   begin
      for I in 1 .. Set.Count loop
         if Set.Sockets (I).Socket = Socket then
            return Set.Sockets (I).Events;
         end if;
      end loop;
      return No_Events;
   end Events;

end GNAT.Sockets.Poll;
