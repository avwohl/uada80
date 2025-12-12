-- Ada.Float_Text_IO for Z80
-- Provides floating-point text I/O operations

with Ada.Text_IO;

package Ada.Float_Text_IO is

   Default_Fore : Ada.Text_IO.Field := 2;
   Default_Aft  : Ada.Text_IO.Field := 6;
   Default_Exp  : Ada.Text_IO.Field := 3;

   -- Output procedures
   procedure Put
     (Item : Float;
      Fore : Ada.Text_IO.Field := Default_Fore;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp);

   procedure Put
     (File : Ada.Text_IO.File_Type;
      Item : Float;
      Fore : Ada.Text_IO.Field := Default_Fore;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp);

   procedure Put
     (To   : out String;
      Item : Float;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp);

   -- Input procedures
   procedure Get
     (Item  : out Float;
      Width : Ada.Text_IO.Field := 0);

   procedure Get
     (File  : Ada.Text_IO.File_Type;
      Item  : out Float;
      Width : Ada.Text_IO.Field := 0);

   procedure Get
     (From : String;
      Item : out Float;
      Last : out Positive);

end Ada.Float_Text_IO;
