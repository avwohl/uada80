-- Ada.Text_IO.Float_Aux for Z80
-- Float I/O support

package Ada.Text_IO.Float_Aux is

   -- Default field widths
   Default_Fore : constant := 2;
   Default_Aft  : constant := 6;
   Default_Exp  : constant := 3;

   -- Get Float from string
   procedure Get_Float
     (From : String;
      Item : out Float;
      Last : out Positive);

   -- Put Float to string
   procedure Put_Float
     (To   : out String;
      Item : Float;
      Aft  : Natural := Default_Aft;
      Exp  : Natural := Default_Exp);

   -- Image of Float with specified format
   function Float_Image
     (Item : Float;
      Fore : Natural := Default_Fore;
      Aft  : Natural := Default_Aft;
      Exp  : Natural := Default_Exp) return String;

end Ada.Text_IO.Float_Aux;
