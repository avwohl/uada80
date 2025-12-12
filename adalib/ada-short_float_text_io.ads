-- Ada.Short_Float_Text_IO for Z80
-- Text I/O for Short_Float type
--
-- Note: On Z80, Short_Float is the same as Float (48-bit z88dk format)

with Ada.Text_IO;

package Ada.Short_Float_Text_IO is
   pragma Elaborate_Body;

   Default_Fore : Ada.Text_IO.Field := 2;
   Default_Aft  : Ada.Text_IO.Field := 4;
   Default_Exp  : Ada.Text_IO.Field := 3;

   procedure Get
     (File  : Ada.Text_IO.File_Type;
      Item  : out Short_Float;
      Width : Ada.Text_IO.Field := 0);

   procedure Get
     (Item  : out Short_Float;
      Width : Ada.Text_IO.Field := 0);

   procedure Put
     (File : Ada.Text_IO.File_Type;
      Item : Short_Float;
      Fore : Ada.Text_IO.Field := Default_Fore;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp);

   procedure Put
     (Item : Short_Float;
      Fore : Ada.Text_IO.Field := Default_Fore;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp);

   procedure Get
     (From : String;
      Item : out Short_Float;
      Last : out Positive);

   procedure Put
     (To   : out String;
      Item : Short_Float;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp);

end Ada.Short_Float_Text_IO;
