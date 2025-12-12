-- GNAT.Table for Z80
-- Generic growable table

generic
   type Table_Component_Type is private;
   type Table_Index_Type is range <>;

   Table_Low_Bound  : Table_Index_Type;
   Table_Initial    : Positive;
   Table_Increment  : Natural;

package GNAT.Table is
   pragma Preelaborate;

   -- Table type
   type Table_Type is array (Table_Index_Type range <>) of
     Table_Component_Type;

   type Table_Ptr is access all Table_Type;

   -- Table instance
   Table : Table_Ptr;

   -- Current bounds
   function First return Table_Index_Type;
   function Last return Table_Index_Type;

   -- Length
   function Length return Natural;

   -- Allocate new entries
   procedure Set_Last (New_Last : Table_Index_Type);
   procedure Increment_Last;
   procedure Decrement_Last;

   -- Append
   procedure Append (New_Val : Table_Component_Type);

   -- Initialize/Free
   procedure Init;
   procedure Free;

   -- Release extra storage
   procedure Release;

end GNAT.Table;
