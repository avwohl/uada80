-- Ada.Task_Attributes for Z80
-- Generic task local storage

with Ada.Task_Identification;

generic
   type Attribute is private;
   Initial_Value : Attribute;
package Ada.Task_Attributes is
   pragma Preelaborate;

   function Value
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task)
     return Attribute;

   function Reference
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task)
     return access Attribute;

   procedure Set_Value
     (Val : Attribute;
      T   : Ada.Task_Identification.Task_Id :=
              Ada.Task_Identification.Current_Task);

   procedure Reinitialize
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task);

end Ada.Task_Attributes;
