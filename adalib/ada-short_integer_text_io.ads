-- Ada.Short_Integer_Text_IO for Z80
-- Text I/O for Short_Integer type
--
-- Note: On Z80, Short_Integer is typically 8-bit

with Ada.Text_IO;

package Ada.Short_Integer_Text_IO is
   pragma Elaborate_Body;

   Default_Width : Ada.Text_IO.Field := 4;
   Default_Base  : Ada.Text_IO.Number_Base := 10;

   procedure Get
     (File  : Ada.Text_IO.File_Type;
      Item  : out Short_Integer;
      Width : Ada.Text_IO.Field := 0);

   procedure Get
     (Item  : out Short_Integer;
      Width : Ada.Text_IO.Field := 0);

   procedure Put
     (File  : Ada.Text_IO.File_Type;
      Item  : Short_Integer;
      Width : Ada.Text_IO.Field := Default_Width;
      Base  : Ada.Text_IO.Number_Base := Default_Base);

   procedure Put
     (Item  : Short_Integer;
      Width : Ada.Text_IO.Field := Default_Width;
      Base  : Ada.Text_IO.Number_Base := Default_Base);

   procedure Get
     (From : String;
      Item : out Short_Integer;
      Last : out Positive);

   procedure Put
     (To   : out String;
      Item : Short_Integer;
      Base : Ada.Text_IO.Number_Base := Default_Base);

end Ada.Short_Integer_Text_IO;
