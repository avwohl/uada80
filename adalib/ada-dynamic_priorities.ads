-- Ada.Dynamic_Priorities for Z80
-- Dynamic task priority management

with System;
with Ada.Task_Identification;

package Ada.Dynamic_Priorities is
   pragma Preelaborate;

   procedure Set_Priority
     (Priority : System.Any_Priority;
      T        : Ada.Task_Identification.Task_Id :=
                   Ada.Task_Identification.Current_Task);

   function Get_Priority
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task)
     return System.Any_Priority;

end Ada.Dynamic_Priorities;
