-- Ada.Containers.Bounded_Priority_Queues body for Z80
-- Bounded priority queue implementation

package body Ada.Containers.Bounded_Priority_Queues is

   package body Implementation is

      -------------
      -- Enqueue --
      -------------

      procedure Enqueue (List : in Out List_Type; New_Item : Queue_Interfaces.Element_Type) is
         Insert_Pos : Count_Type := List.Count + 1;
         New_Priority : constant Queue_Priority := Get_Priority (New_Item);
      begin
         if List.Count >= List.Capacity then
            raise Capacity_Error;
         end if;

         -- Find insertion position (maintain sorted order by priority)
         for I in 1 .. List.Count loop
            if Before (New_Priority, Get_Priority (List.Elements (I))) then
               Insert_Pos := I;
               exit;
            end if;
         end loop;

         -- Shift elements to make room
         for I in reverse Insert_Pos .. List.Count loop
            List.Elements (I + 1) := List.Elements (I);
         end loop;

         List.Elements (Insert_Pos) := New_Item;
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

         -- Dequeue highest priority (first element)
         Element := List.Elements (1);
         for I in 1 .. List.Count - 1 loop
            List.Elements (I) := List.Elements (I + 1);
         end loop;
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

end Ada.Containers.Bounded_Priority_Queues;
