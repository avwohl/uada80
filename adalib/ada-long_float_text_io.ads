-- Ada.Long_Float_Text_IO for Z80
-- Text I/O for Long_Float type
--
-- Note: On Z80, Long_Float is the same as Float (48-bit z88dk format)

with Ada.Text_IO;

package Ada.Long_Float_Text_IO is
   pragma Elaborate_Body;

   Default_Fore : Ada.Text_IO.Field := 2;
   Default_Aft  : Ada.Text_IO.Field := 6;
   Default_Exp  : Ada.Text_IO.Field := 3;

   procedure Get
     (File  : Ada.Text_IO.File_Type;
      Item  : out Long_Float;
      Width : Ada.Text_IO.Field := 0);

   procedure Get
     (Item  : out Long_Float;
      Width : Ada.Text_IO.Field := 0);

   procedure Put
     (File : Ada.Text_IO.File_Type;
      Item : Long_Float;
      Fore : Ada.Text_IO.Field := Default_Fore;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp);

   procedure Put
     (Item : Long_Float;
      Fore : Ada.Text_IO.Field := Default_Fore;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp);

   procedure Get
     (From : String;
      Item : out Long_Float;
      Last : out Positive);

   procedure Put
     (To   : out String;
      Item : Long_Float;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp);

end Ada.Long_Float_Text_IO;
