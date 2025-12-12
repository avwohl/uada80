-- GNAT.Sockets.Poll for Z80
-- Socket polling (stub for CP/M)

package GNAT.Sockets.Poll is

   type Poll_Set is limited private;

   type Event_Type is (Input, Output, Error);
   type Event_Set is array (Event_Type) of Boolean;

   No_Events : constant Event_Set := (others => False);

   procedure Initialize (Set : out Poll_Set);
   procedure Finalize (Set : in Out Poll_Set);

   procedure Add_Socket
     (Set    : in Out Poll_Set;
      Socket : Socket_Type;
      Events : Event_Set);

   procedure Remove_Socket
     (Set    : in Out Poll_Set;
      Socket : Socket_Type);

   procedure Wait
     (Set     : Poll_Set;
      Timeout : Duration;
      Count   : out Natural);

   function Events
     (Set    : Poll_Set;
      Socket : Socket_Type) return Event_Set;

private
   Max_Sockets : constant := 8;

   type Socket_Entry is record
      Socket : Socket_Type;
      Events : Event_Set := No_Events;
      Active : Boolean := False;
   end record;

   type Socket_Array is array (1 .. Max_Sockets) of Socket_Entry;

   type Poll_Set is record
      Sockets : Socket_Array;
      Count   : Natural := 0;
   end record;

end GNAT.Sockets.Poll;
