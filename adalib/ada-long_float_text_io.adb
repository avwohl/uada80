-- Ada.Long_Float_Text_IO body for Z80
-- Text I/O for Long_Float type implementation
--
-- Delegates to Float_Text_IO since Long_Float = Float on Z80

with Ada.Float_Text_IO;

package body Ada.Long_Float_Text_IO is

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : Ada.Text_IO.File_Type;
      Item  : out Long_Float;
      Width : Ada.Text_IO.Field := 0)
   is
      F_Item : Float;
   begin
      Ada.Float_Text_IO.Get (File, F_Item, Width);
      Item := Long_Float (F_Item);
   end Get;

   procedure Get
     (Item  : out Long_Float;
      Width : Ada.Text_IO.Field := 0)
   is
      F_Item : Float;
   begin
      Ada.Float_Text_IO.Get (F_Item, Width);
      Item := Long_Float (F_Item);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : Ada.Text_IO.File_Type;
      Item : Long_Float;
      Fore : Ada.Text_IO.Field := Default_Fore;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp)
   is
   begin
      Ada.Float_Text_IO.Put (File, Float (Item), Fore, Aft, Exp);
   end Put;

   procedure Put
     (Item : Long_Float;
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
      Item : out Long_Float;
      Last : out Positive)
   is
      F_Item : Float;
   begin
      Ada.Float_Text_IO.Get (From, F_Item, Last);
      Item := Long_Float (F_Item);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (To   : out String;
      Item : Long_Float;
      Aft  : Ada.Text_IO.Field := Default_Aft;
      Exp  : Ada.Text_IO.Field := Default_Exp)
   is
   begin
      Ada.Float_Text_IO.Put (To, Float (Item), Aft, Exp);
   end Put;

end Ada.Long_Float_Text_IO;
