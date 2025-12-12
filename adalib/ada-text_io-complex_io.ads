-- Ada.Text_IO.Complex_IO for Z80
-- Text I/O for Complex numbers

with Ada.Numerics.Generic_Complex_Types;

generic
   with package Complex_Types is new Ada.Numerics.Generic_Complex_Types (<>);
package Ada.Text_IO.Complex_IO is

   use Complex_Types;

   Default_Fore : Field := 2;
   Default_Aft  : Field := 6;
   Default_Exp  : Field := 3;

   procedure Get
     (File  : File_Type;
      Item  : out Complex;
      Width : Field := 0);

   procedure Get
     (Item  : out Complex;
      Width : Field := 0);

   procedure Put
     (File : File_Type;
      Item : Complex;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp);

   procedure Put
     (Item : Complex;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp);

   procedure Get
     (From : String;
      Item : out Complex;
      Last : out Positive);

   procedure Put
     (To   : out String;
      Item : Complex;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp);

end Ada.Text_IO.Complex_IO;
