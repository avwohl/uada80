-- GNAT.Spitbol.Table_Integer for Z80
-- SPITBOL-style integer table

with GNAT.Spitbol;

package GNAT.Spitbol.Table_Integer is

   type Table is private;

   function Get (T : Table; Key : String) return Integer;
   --  Get value for key, returns 0 if not found

   procedure Set (T : in Out Table; Key : String; Value : Integer);
   --  Set value for key

   function Present (T : Table; Key : String) return Boolean;
   --  Check if key exists

   procedure Delete (T : in Out Table; Key : String);
   --  Remove key from table

   procedure Clear (T : in Out Table);
   --  Remove all entries

   function Size (T : Table) return Natural;
   --  Number of entries

private
   Max_Entries : constant := 100;
   Max_Key_Len : constant := 32;

   type Entry_Type is record
      Key     : String (1 .. Max_Key_Len);
      Key_Len : Natural := 0;
      Value   : Integer := 0;
      Used    : Boolean := False;
   end record;

   type Entry_Array is array (1 .. Max_Entries) of Entry_Type;

   type Table is record
      Entries : Entry_Array;
      Count   : Natural := 0;
   end record;

end GNAT.Spitbol.Table_Integer;
