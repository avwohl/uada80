-- Ada.Text_IO.Modular_Aux for Z80
-- Modular type I/O support

package Ada.Text_IO.Modular_Aux is

   type Unsigned is mod 2 ** 16;

   -- Default field widths
   Default_Width : constant := 6;
   Default_Base  : constant := 10;

   -- Get modular from string
   procedure Get_Unsigned
     (From : String;
      Item : out Unsigned;
      Last : out Positive);

   -- Put modular to string
   procedure Put_Unsigned
     (To   : out String;
      Item : Unsigned;
      Base : Natural := Default_Base);

   -- Image of modular with specified format
   function Unsigned_Image
     (Item  : Unsigned;
      Width : Natural := 0;
      Base  : Natural := Default_Base) return String;

end Ada.Text_IO.Modular_Aux;
