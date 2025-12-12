-- Ada.Containers.Synchronized_Queue_Interfaces for Z80
-- Interface for synchronized queues

generic
   type Element_Type is private;
package Ada.Containers.Synchronized_Queue_Interfaces is
   pragma Pure;

   type Queue is synchronized interface;

   procedure Enqueue
     (Container : in out Queue;
      New_Item  : Element_Type) is abstract
     with Synchronization => By_Entry;

   procedure Dequeue
     (Container : in Out Queue;
      Element   : out Element_Type) is abstract
     with Synchronization => By_Entry;

   function Current_Use (Container : Queue) return Count_Type is abstract;
   function Peak_Use (Container : Queue) return Count_Type is abstract;

end Ada.Containers.Synchronized_Queue_Interfaces;
