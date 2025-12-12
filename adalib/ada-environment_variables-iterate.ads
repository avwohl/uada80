-- Ada.Environment_Variables.Iterate for Z80
-- Environment variable iteration

package Ada.Environment_Variables.Iterate is

   type Cursor is private;

   No_Element : constant Cursor;

   function First return Cursor;
   function Next (Position : Cursor) return Cursor;
   function Has_Element (Position : Cursor) return Boolean;

   function Name (Position : Cursor) return String;
   function Value (Position : Cursor) return String;

private
   type Cursor is record
      Index : Natural := 0;
   end record;

   No_Element : constant Cursor := (Index => 0);

end Ada.Environment_Variables.Iterate;
