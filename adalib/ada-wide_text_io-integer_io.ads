-- Ada.Wide_Text_IO.Integer_IO for Z80
-- Wide integer I/O operations

with Ada.Wide_Text_IO;

generic
   type Num is range <>;
package Ada.Wide_Text_IO.Integer_IO is

   Default_Width : Field := Num'Width;
   Default_Base  : Number_Base := 10;

   procedure Get
     (File  : File_Type;
      Item  : out Num;
      Width : Field := 0);

   procedure Get
     (Item  : out Num;
      Width : Field := 0);

   procedure Put
     (File  : File_Type;
      Item  : Num;
      Width : Field := Default_Width;
      Base  : Number_Base := Default_Base);

   procedure Put
     (Item  : Num;
      Width : Field := Default_Width;
      Base  : Number_Base := Default_Base);

   procedure Get
     (From : Wide_String;
      Item : out Num;
      Last : out Positive);

   procedure Put
     (To   : out Wide_String;
      Item : Num;
      Base : Number_Base := Default_Base);

end Ada.Wide_Text_IO.Integer_IO;
