-- GNAT.Registry body for Z80
-- Windows registry stubs

package body GNAT.Registry is

   Not_Supported : constant String := "Registry not supported on CP/M";

   ----------------
   -- Key_Exists --
   ----------------

   function Key_Exists (Key : HKEY; Sub_Key : String) return Boolean is
      pragma Unreferenced (Key, Sub_Key);
   begin
      return False;
   end Key_Exists;

   -----------------
   -- Query_Value --
   -----------------

   function Query_Value (Key : HKEY; Sub_Key : String; Name : String) return String is
      pragma Unreferenced (Key, Sub_Key, Name);
   begin
      raise Registry_Error with Not_Supported;
      return "";
   end Query_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Key     : HKEY;
      Sub_Key : String;
      Name    : String;
      Value   : String)
   is
      pragma Unreferenced (Key, Sub_Key, Name, Value);
   begin
      raise Registry_Error with Not_Supported;
   end Set_Value;

   ----------------
   -- Delete_Key --
   ----------------

   procedure Delete_Key (Key : HKEY; Sub_Key : String) is
      pragma Unreferenced (Key, Sub_Key);
   begin
      raise Registry_Error with Not_Supported;
   end Delete_Key;

   ------------------
   -- Delete_Value --
   ------------------

   procedure Delete_Value (Key : HKEY; Sub_Key : String; Name : String) is
      pragma Unreferenced (Key, Sub_Key, Name);
   begin
      raise Registry_Error with Not_Supported;
   end Delete_Value;

end GNAT.Registry;
