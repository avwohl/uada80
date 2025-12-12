-- Ada.Strings.Text_Buffers for Z80
-- Text buffer abstraction for Put_Image

package Ada.Strings.Text_Buffers is
   pragma Preelaborate;

   type Text_Buffer_Count is range 0 .. 2**16 - 1;

   New_Line_Count : constant Text_Buffer_Count := 1;

   type Root_Buffer_Type is abstract tagged limited private;

   procedure Put (Buffer : in Out Root_Buffer_Type; Item : String) is abstract;

   procedure Wide_Put (Buffer : in Out Root_Buffer_Type; Item : Wide_String) is abstract;

   procedure Wide_Wide_Put (Buffer : in Out Root_Buffer_Type; Item : Wide_Wide_String) is abstract;

   procedure Put_UTF_8 (Buffer : in Out Root_Buffer_Type; Item : String) is abstract;

   procedure New_Line (Buffer : in Out Root_Buffer_Type) is abstract;

   function Current_Indent (Buffer : Root_Buffer_Type) return Text_Buffer_Count;

   procedure Increase_Indent
     (Buffer : in Out Root_Buffer_Type;
      Amount : Text_Buffer_Count := Standard_Indent);

   procedure Decrease_Indent
     (Buffer : in Out Root_Buffer_Type;
      Amount : Text_Buffer_Count := Standard_Indent);

   Standard_Indent : constant Text_Buffer_Count := 3;

private

   type Root_Buffer_Type is abstract tagged limited record
      Indent_Level : Text_Buffer_Count := 0;
   end record;

end Ada.Strings.Text_Buffers;
