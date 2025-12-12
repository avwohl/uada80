-- GNAT.Table body for Z80
-- Generic growable table implementation

with Ada.Unchecked_Deallocation;

package body GNAT.Table is

   Current_Last : Table_Index_Type := Table_Low_Bound - 1;
   Allocated    : Natural := 0;

   procedure Free_Table is new Ada.Unchecked_Deallocation
     (Table_Type, Table_Ptr);

   -----------
   -- First --
   -----------

   function First return Table_Index_Type is
   begin
      return Table_Low_Bound;
   end First;

   ----------
   -- Last --
   ----------

   function Last return Table_Index_Type is
   begin
      return Current_Last;
   end Last;

   ------------
   -- Length --
   ------------

   function Length return Natural is
   begin
      if Current_Last < Table_Low_Bound then
         return 0;
      else
         return Natural (Current_Last - Table_Low_Bound + 1);
      end if;
   end Length;

   --------------
   -- Set_Last --
   --------------

   procedure Set_Last (New_Last : Table_Index_Type) is
      New_Size : Natural;
      New_Table : Table_Ptr;
   begin
      if New_Last > Current_Last then
         -- Need to grow
         New_Size := Natural (New_Last - Table_Low_Bound + 1);
         if New_Size > Allocated then
            -- Allocate more space
            if Allocated = 0 then
               Allocated := Table_Initial;
            end if;
            while Allocated < New_Size loop
               Allocated := Allocated + Table_Increment;
               if Table_Increment = 0 then
                  Allocated := Allocated * 2;
               end if;
            end loop;

            New_Table := new Table_Type
              (Table_Low_Bound .. Table_Low_Bound + Table_Index_Type (Allocated) - 1);

            if Table /= null then
               New_Table (Table'Range) := Table.all;
               Free_Table (Table);
            end if;
            Table := New_Table;
         end if;
      end if;
      Current_Last := New_Last;
   end Set_Last;

   --------------------
   -- Increment_Last --
   --------------------

   procedure Increment_Last is
   begin
      Set_Last (Current_Last + 1);
   end Increment_Last;

   --------------------
   -- Decrement_Last --
   --------------------

   procedure Decrement_Last is
   begin
      Current_Last := Current_Last - 1;
   end Decrement_Last;

   ------------
   -- Append --
   ------------

   procedure Append (New_Val : Table_Component_Type) is
   begin
      Increment_Last;
      Table (Current_Last) := New_Val;
   end Append;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Free;
   end Init;

   ----------
   -- Free --
   ----------

   procedure Free is
   begin
      if Table /= null then
         Free_Table (Table);
      end if;
      Current_Last := Table_Low_Bound - 1;
      Allocated := 0;
   end Free;

   -------------
   -- Release --
   -------------

   procedure Release is
   begin
      -- Could shrink allocation here
      null;
   end Release;

end GNAT.Table;
