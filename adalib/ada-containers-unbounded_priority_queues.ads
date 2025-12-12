-- Ada.Containers.Unbounded_Priority_Queues for Z80
-- Unbounded priority queue implementation

with Ada.Containers.Synchronized_Queue_Interfaces;

generic
   with package Queue_Interfaces is
     new Ada.Containers.Synchronized_Queue_Interfaces (<>);
   type Queue_Priority is private;
   with function Get_Priority
     (Element : Queue_Interfaces.Element_Type) return Queue_Priority is <>;
   with function Before
     (Left, Right : Queue_Priority) return Boolean is <>;
   Default_Ceiling : System.Any_Priority := System.Priority'Last;
package Ada.Containers.Unbounded_Priority_Queues is
   pragma Preelaborate;

   -- Maximum capacity for Z80
   Max_Capacity : constant := 32;

   package Implementation is
      type List_Type is tagged limited private;

      procedure Enqueue (List : in Out List_Type; New_Item : Queue_Interfaces.Element_Type);
      procedure Dequeue (List : in Out List_Type; Element : out Queue_Interfaces.Element_Type);
      procedure Dequeue (List : in Out List_Type;
                        At_Head : Boolean;
                        Element : out Queue_Interfaces.Element_Type);
      function Length (List : List_Type) return Count_Type;
      function Max_Length (List : List_Type) return Count_Type;

   private
      type Element_Array is array (1 .. Max_Capacity) of Queue_Interfaces.Element_Type;

      type List_Type is tagged limited record
         Elements   : Element_Array;
         Count      : Count_Type := 0;
         Peak_Count : Count_Type := 0;
      end record;
   end Implementation;

   protected type Queue (Ceiling : System.Any_Priority := Default_Ceiling)
     with Priority => Ceiling
     is new Queue_Interfaces.Queue with

      overriding entry Enqueue (New_Item : Queue_Interfaces.Element_Type);
      overriding entry Dequeue (Element : out Queue_Interfaces.Element_Type);

      -- Additional priority queue operations
      entry Dequeue_Only_High_Priority
        (At_Least : Queue_Priority;
         Element  : out Queue_Interfaces.Element_Type;
         Success  : out Boolean);

      overriding function Current_Use return Count_Type;
      overriding function Peak_Use return Count_Type;

   private
      List : Implementation.List_Type;
   end Queue;

end Ada.Containers.Unbounded_Priority_Queues;
