-- Ada.Environment_Variables.Iterate body for Z80
-- Environment variable iteration (stub for CP/M)

package body Ada.Environment_Variables.Iterate is

   -----------
   -- First --
   -----------

   function First return Cursor is
   begin
      -- CP/M has no environment variables
      return No_Element;
   end First;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
      pragma Unreferenced (Position);
   begin
      return No_Element;
   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Index > 0;
   end Has_Element;

   ----------
   -- Name --
   ----------

   function Name (Position : Cursor) return String is
      pragma Unreferenced (Position);
   begin
      return "";
   end Name;

   -----------
   -- Value --
   -----------

   function Value (Position : Cursor) return String is
      pragma Unreferenced (Position);
   begin
      return "";
   end Value;

end Ada.Environment_Variables.Iterate;
