-- Ada.Short_Integer_Text_IO body for Z80
-- Text I/O for Short_Integer implementation

with Ada.Integer_Text_IO;

package body Ada.Short_Integer_Text_IO is

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : Ada.Text_IO.File_Type;
      Item  : out Short_Integer;
      Width : Ada.Text_IO.Field := 0)
   is
      Temp : Integer;
   begin
      Ada.Integer_Text_IO.Get (File, Temp, Width);
      Item := Short_Integer (Temp);
   end Get;

   procedure Get
     (Item  : out Short_Integer;
      Width : Ada.Text_IO.Field := 0)
   is
      Temp : Integer;
   begin
      Ada.Integer_Text_IO.Get (Temp, Width);
      Item := Short_Integer (Temp);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (File  : Ada.Text_IO.File_Type;
      Item  : Short_Integer;
      Width : Ada.Text_IO.Field := Default_Width;
      Base  : Ada.Text_IO.Number_Base := Default_Base)
   is
   begin
      Ada.Integer_Text_IO.Put (File, Integer (Item), Width, Base);
   end Put;

   procedure Put
     (Item  : Short_Integer;
      Width : Ada.Text_IO.Field := Default_Width;
      Base  : Ada.Text_IO.Number_Base := Default_Base)
   is
   begin
      Ada.Integer_Text_IO.Put (Integer (Item), Width, Base);
   end Put;

   ---------
   -- Get --
   ---------

   procedure Get
     (From : String;
      Item : out Short_Integer;
      Last : out Positive)
   is
      Temp : Integer;
   begin
      Ada.Integer_Text_IO.Get (From, Temp, Last);
      Item := Short_Integer (Temp);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (To   : out String;
      Item : Short_Integer;
      Base : Ada.Text_IO.Number_Base := Default_Base)
   is
   begin
      Ada.Integer_Text_IO.Put (To, Integer (Item), Base);
   end Put;

end Ada.Short_Integer_Text_IO;
