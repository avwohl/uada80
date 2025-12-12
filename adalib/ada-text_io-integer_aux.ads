-- Ada.Text_IO.Integer_Aux for Z80
-- Integer I/O support

package Ada.Text_IO.Integer_Aux is

   -- Default field widths
   Default_Width : constant := 11;
   Default_Base  : constant := 10;

   -- Get Integer from string
   procedure Get_Integer
     (From : String;
      Item : out Integer;
      Last : out Positive);

   -- Put Integer to string
   procedure Put_Integer
     (To    : out String;
      Item  : Integer;
      Base  : Natural := Default_Base);

   -- Put Integer with specified width
   procedure Put_Width
     (To    : out String;
      Item  : Integer;
      Width : Natural;
      Base  : Natural := Default_Base);

   -- Image of Integer with specified format
   function Integer_Image
     (Item  : Integer;
      Width : Natural := 0;
      Base  : Natural := Default_Base) return String;

end Ada.Text_IO.Integer_Aux;
