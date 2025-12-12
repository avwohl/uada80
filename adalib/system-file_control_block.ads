-- System.File_Control_Block for Z80/CP/M
-- CP/M File Control Block definition

package System.File_Control_Block is
   pragma Pure;

   -- CP/M FCB structure
   type FCB is record
      Drive     : Natural;       -- Drive code (0=default, 1=A, etc.)
      Name      : String (1 .. 8);   -- Filename (space padded)
      Extension : String (1 .. 3);   -- Extension (space padded)
      Extent    : Natural;       -- Current extent
      S1        : Natural;       -- Reserved
      S2        : Natural;       -- Reserved
      RC        : Natural;       -- Record count in extent
      D         : String (1 .. 16);  -- Allocation map
      CR        : Natural;       -- Current record
      R0        : Natural;       -- Random record low
      R1        : Natural;       -- Random record mid
      R2        : Natural;       -- Random record high
   end record;

   -- FCB size
   FCB_Size : constant := 36;

   -- Initialize FCB with filename
   procedure Init_FCB (F : out FCB; Filename : String);

   -- Parse filename into name and extension
   procedure Parse_Filename
     (Filename : String;
      Name     : out String;
      Ext      : out String);

   -- Clear FCB to defaults
   procedure Clear_FCB (F : out FCB);

end System.File_Control_Block;
