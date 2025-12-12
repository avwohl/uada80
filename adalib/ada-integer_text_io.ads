-- Ada.Integer_Text_IO specification for Z80
-- Integer I/O operations for Standard.Integer
--
-- This is an instantiation of Ada.Text_IO.Integer_IO for Integer

with Ada.Text_IO;

package Ada.Integer_Text_IO is

   Default_Width : Ada.Text_IO.Field := 11;
   Default_Base  : Ada.Text_IO.Number_Base := 10;

   procedure Get
     (File  : Ada.Text_IO.File_Type;
      Item  : out Integer;
      Width : Ada.Text_IO.Field := 0);

   procedure Get
     (Item  : out Integer;
      Width : Ada.Text_IO.Field := 0);

   procedure Put
     (File  : Ada.Text_IO.File_Type;
      Item  : Integer;
      Width : Ada.Text_IO.Field := Default_Width;
      Base  : Ada.Text_IO.Number_Base := Default_Base);

   procedure Put
     (Item  : Integer;
      Width : Ada.Text_IO.Field := Default_Width;
      Base  : Ada.Text_IO.Number_Base := Default_Base);

   procedure Get
     (From : String;
      Item : out Integer;
      Last : out Positive);

   procedure Put
     (To   : out String;
      Item : Integer;
      Base : Ada.Text_IO.Number_Base := Default_Base);

end Ada.Integer_Text_IO;
