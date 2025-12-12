-- Ada.Task_Attributes body for Z80
-- Generic task local storage implementation

with System.Tasking;

package body Ada.Task_Attributes is

   -- Task-indexed attribute storage (Max_Tasks = 8)
   Values : array (1 .. 8) of aliased Attribute :=
     (others => Initial_Value);

   -----------
   -- Value --
   -----------

   function Value
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task)
     return Attribute
   is
      Index : constant Natural := Natural (System.Tasking.To_Task_Id (T));
   begin
      if Index in Values'Range then
         return Values (Index);
      else
         return Initial_Value;
      end if;
   end Value;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task)
     return access Attribute
   is
      Index : constant Natural := Natural (System.Tasking.To_Task_Id (T));
   begin
      if Index in Values'Range then
         return Values (Index)'Access;
      else
         -- Return first slot as fallback
         return Values (1)'Access;
      end if;
   end Reference;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Val : Attribute;
      T   : Ada.Task_Identification.Task_Id :=
              Ada.Task_Identification.Current_Task)
   is
      Index : constant Natural := Natural (System.Tasking.To_Task_Id (T));
   begin
      if Index in Values'Range then
         Values (Index) := Val;
      end if;
   end Set_Value;

   ------------------
   -- Reinitialize --
   ------------------

   procedure Reinitialize
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task)
   is
      Index : constant Natural := Natural (System.Tasking.To_Task_Id (T));
   begin
      if Index in Values'Range then
         Values (Index) := Initial_Value;
      end if;
   end Reinitialize;

end Ada.Task_Attributes;
