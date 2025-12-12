-- Ada.Strings.Text_Buffers body for Z80
-- Text buffer abstraction implementation

package body Ada.Strings.Text_Buffers is

   --------------------
   -- Current_Indent --
   --------------------

   function Current_Indent (Buffer : Root_Buffer_Type) return Text_Buffer_Count is
   begin
      return Buffer.Indent_Level;
   end Current_Indent;

   ---------------------
   -- Increase_Indent --
   ---------------------

   procedure Increase_Indent
     (Buffer : in Out Root_Buffer_Type;
      Amount : Text_Buffer_Count := Standard_Indent)
   is
   begin
      if Buffer.Indent_Level <= Text_Buffer_Count'Last - Amount then
         Buffer.Indent_Level := Buffer.Indent_Level + Amount;
      else
         Buffer.Indent_Level := Text_Buffer_Count'Last;
      end if;
   end Increase_Indent;

   ---------------------
   -- Decrease_Indent --
   ---------------------

   procedure Decrease_Indent
     (Buffer : in Out Root_Buffer_Type;
      Amount : Text_Buffer_Count := Standard_Indent)
   is
   begin
      if Buffer.Indent_Level >= Amount then
         Buffer.Indent_Level := Buffer.Indent_Level - Amount;
      else
         Buffer.Indent_Level := 0;
      end if;
   end Decrease_Indent;

end Ada.Strings.Text_Buffers;
