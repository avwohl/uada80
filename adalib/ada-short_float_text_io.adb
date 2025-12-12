-- Ada.Short_Float_Text_IO body for Z80
-- Text I/O for Short_Float type implementation
--
-- Delegates to Float_Text_IO since Short_Float = Float on Z80

with Ada.Float_Text_IO;

package body Ada.Short_Float_Text_IO is

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : Ada.Text_IO.File_Type;
      Item  : out Short_Float;
      Width : Ada.Text_IO.Field := 0)
   is
      F_Item : Float;
   begin
      Ada.Float_Text_IO.Get (File, F_Item, Width);
      Item := Short_Float (F_Item);
   end Get;

   procedure Get
     (Item  : out Short_Float;
      Width : Ada.Text_IO.Field := 0)
   is
      F_Item : Float;
   begin
      Ada.Float_Text_IO.Get (F_Item, Width);
      Item := Short_Float (F_Item);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : Ada.Text_IO.File_Type;
      Item : Short_Float;
      Fore : Ada.Text_IO.Field := Default_Fore;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp)
   is
   begin
      Ada.Float_Text_IO.Put (File, Float (Item), Fore, Aft, Exp);
   end Put;

   procedure Put
     (Item : Short_Float;
      Fore : Ada.Text_IO.Field := Default_Fore;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp)
   is
   begin
      Ada.Float_Text_IO.Put (Float (Item), Fore, Aft, Exp);
   end Put;

   ---------
   -- Get --
   ---------

   procedure Get
     (From : String;
      Item : out Short_Float;
      Last : out Positive)
   is
      F_Item : Float;
   begin
      Ada.Float_Text_IO.Get (From, F_Item, Last);
      Item := Short_Float (F_Item);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (To   : out String;
      Item : Short_Float;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp)
   is
   begin
      Ada.Float_Text_IO.Put (To, Float (Item), Aft, Exp);
   end Put;

end Ada.Short_Float_Text_IO;
