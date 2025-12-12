-- Ada.Containers.Bounded_Synchronized_Queues body for Z80
-- Bounded synchronized queue implementation

package body Ada.Containers.Bounded_Synchronized_Queues is

   package body Implementation is

      -------------
      -- Enqueue --
      -------------

      procedure Enqueue (List : in Out List_Type; New_Item : Queue_Interfaces.Element_Type) is
      begin
         if List.Count >= List.Capacity then
            raise Capacity_Error;
         end if;

         List.Elements (List.Tail) := New_Item;
         if List.Tail >= List.Capacity then
            List.Tail := 1;
         else
            List.Tail := List.Tail + 1;
         end if;
         List.Count := List.Count + 1;

         if List.Count > List.Peak_Count then
            List.Peak_Count := List.Count;
         end if;
      end Enqueue;

      -------------
      -- Dequeue --
      -------------

      procedure Dequeue (List : in Out List_Type; Element : out Queue_Interfaces.Element_Type) is
      begin
         if List.Count = 0 then
            raise Constraint_Error;
         end if;

         Element := List.Elements (List.Head);
         if List.Head >= List.Capacity then
            List.Head := 1;
         else
            List.Head := List.Head + 1;
         end if;
         List.Count := List.Count - 1;
      end Dequeue;

      ------------
      -- Length --
      ------------

      function Length (List : List_Type) return Count_Type is
      begin
         return List.Count;
      end Length;

      ----------------
      -- Max_Length --
      ----------------

      function Max_Length (List : List_Type) return Count_Type is
      begin
         return List.Peak_Count;
      end Max_Length;

   end Implementation;

   --------------------
   -- Protected Queue --
   --------------------

   protected body Queue is

      -------------
      -- Enqueue --
      -------------

      entry Enqueue (New_Item : Queue_Interfaces.Element_Type)
        when List.Length < Capacity
      is
      begin
         List.Enqueue (New_Item);
      end Enqueue;

      -------------
      -- Dequeue --
      -------------

      entry Dequeue (Element : out Queue_Interfaces.Element_Type)
        when List.Length > 0
      is
      begin
         List.Dequeue (Element);
      end Dequeue;

      -----------------
      -- Current_Use --
      -----------------

      function Current_Use return Count_Type is
      begin
         return List.Length;
      end Current_Use;

      --------------
      -- Peak_Use --
      --------------

      function Peak_Use return Count_Type is
      begin
         return List.Max_Length;
      end Peak_Use;

   end Queue;

end Ada.Containers.Bounded_Synchronized_Queues;
