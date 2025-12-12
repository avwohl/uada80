-- Ada.Long_Long_Integer_Text_IO for Z80
-- Text I/O for Long_Long_Integer type
--
-- Note: On Z80, Long_Long_Integer is typically 64-bit (emulated)

with Ada.Text_IO;

package Ada.Long_Long_Integer_Text_IO is
   pragma Elaborate_Body;

   Default_Width : Ada.Text_IO.Field := 21;
   Default_Base  : Ada.Text_IO.Number_Base := 10;

   procedure Get
     (File  : Ada.Text_IO.File_Type;
      Item  : out Long_Long_Integer;
      Width : Ada.Text_IO.Field := 0);

   procedure Get
     (Item  : out Long_Long_Integer;
      Width : Ada.Text_IO.Field := 0);

   procedure Put
     (File  : Ada.Text_IO.File_Type;
      Item  : Long_Long_Integer;
      Width : Ada.Text_IO.Field := Default_Width;
      Base  : Ada.Text_IO.Number_Base := Default_Base);

   procedure Put
     (Item  : Long_Long_Integer;
      Width : Ada.Text_IO.Field := Default_Width;
      Base  : Ada.Text_IO.Number_Base := Default_Base);

   procedure Get
     (From : String;
      Item : out Long_Long_Integer;
      Last : out Positive);

   procedure Put
     (To   : out String;
      Item : Long_Long_Integer;
      Base : Ada.Text_IO.Number_Base := Default_Base);

end Ada.Long_Long_Integer_Text_IO;
