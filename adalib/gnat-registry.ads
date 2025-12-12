-- GNAT.Registry for Z80
-- Windows registry interface (stub for CP/M)

package GNAT.Registry is

   -- Registry types (stubs - not supported on CP/M)
   type HKEY is private;

   HKEY_CLASSES_ROOT     : constant HKEY;
   HKEY_CURRENT_USER     : constant HKEY;
   HKEY_LOCAL_MACHINE    : constant HKEY;
   HKEY_USERS            : constant HKEY;
   HKEY_CURRENT_CONFIG   : constant HKEY;

   type Key_Mode is (Read_Only, Read_Write);

   Registry_Error : exception;

   -- All operations raise Registry_Error on CP/M
   function Key_Exists (Key : HKEY; Sub_Key : String) return Boolean;
   function Query_Value (Key : HKEY; Sub_Key : String; Name : String) return String;

   procedure Set_Value
     (Key     : HKEY;
      Sub_Key : String;
      Name    : String;
      Value   : String);

   procedure Delete_Key (Key : HKEY; Sub_Key : String);
   procedure Delete_Value (Key : HKEY; Sub_Key : String; Name : String);

private
   type HKEY is new Natural;

   HKEY_CLASSES_ROOT   : constant HKEY := 1;
   HKEY_CURRENT_USER   : constant HKEY := 2;
   HKEY_LOCAL_MACHINE  : constant HKEY := 3;
   HKEY_USERS          : constant HKEY := 4;
   HKEY_CURRENT_CONFIG : constant HKEY := 5;

end GNAT.Registry;
