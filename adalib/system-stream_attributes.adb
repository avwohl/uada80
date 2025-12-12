-- System.Stream_Attributes body for Z80
-- Stream attribute implementation

package body System.Stream_Attributes is

   use Ada.Streams;

   -- Integer input
   procedure I_I (Stream : not null access Root_Stream_Type'Class;
                  Item   : out Integer)
   is
      Buf : Stream_Element_Array (1 .. 2);
      Last : Stream_Element_Offset;
   begin
      Read (Stream.all, Buf, Last);
      Item := Integer (Buf (1)) + Integer (Buf (2)) * 256;
   end I_I;

   procedure I_SI (Stream : not null access Root_Stream_Type'Class;
                   Item   : out Short_Integer)
   is
      Buf : Stream_Element_Array (1 .. 2);
      Last : Stream_Element_Offset;
   begin
      Read (Stream.all, Buf, Last);
      Item := Short_Integer (Buf (1)) + Short_Integer (Buf (2)) * 256;
   end I_SI;

   procedure I_LI (Stream : not null access Root_Stream_Type'Class;
                   Item   : out Long_Integer)
   is
      Buf : Stream_Element_Array (1 .. 4);
      Last : Stream_Element_Offset;
   begin
      Read (Stream.all, Buf, Last);
      Item := Long_Integer (Buf (1)) + Long_Integer (Buf (2)) * 256 +
              Long_Integer (Buf (3)) * 65536 + Long_Integer (Buf (4)) * 16777216;
   end I_LI;

   procedure I_LLI (Stream : not null access Root_Stream_Type'Class;
                    Item   : out Long_Long_Integer)
   is
      Buf : Stream_Element_Array (1 .. 8);
      Last : Stream_Element_Offset;
      Factor : Long_Long_Integer := 1;
   begin
      Read (Stream.all, Buf, Last);
      Item := 0;
      for I in Buf'Range loop
         Item := Item + Long_Long_Integer (Buf (I)) * Factor;
         Factor := Factor * 256;
      end loop;
   end I_LLI;

   -- Integer output
   procedure W_I (Stream : not null access Root_Stream_Type'Class;
                  Item   : Integer)
   is
      Buf : Stream_Element_Array (1 .. 2);
      Val : Integer := Item;
   begin
      Buf (1) := Stream_Element (Val mod 256);
      Buf (2) := Stream_Element (Val / 256);
      Write (Stream.all, Buf);
   end W_I;

   procedure W_SI (Stream : not null access Root_Stream_Type'Class;
                   Item   : Short_Integer)
   is
      Buf : Stream_Element_Array (1 .. 2);
      Val : Short_Integer := Item;
   begin
      Buf (1) := Stream_Element (Val mod 256);
      Buf (2) := Stream_Element (Val / 256);
      Write (Stream.all, Buf);
   end W_SI;

   procedure W_LI (Stream : not null access Root_Stream_Type'Class;
                   Item   : Long_Integer)
   is
      Buf : Stream_Element_Array (1 .. 4);
      Val : Long_Integer := Item;
   begin
      for I in Buf'Range loop
         Buf (I) := Stream_Element (Val mod 256);
         Val := Val / 256;
      end loop;
      Write (Stream.all, Buf);
   end W_LI;

   procedure W_LLI (Stream : not null access Root_Stream_Type'Class;
                    Item   : Long_Long_Integer)
   is
      Buf : Stream_Element_Array (1 .. 8);
      Val : Long_Long_Integer := Item;
   begin
      for I in Buf'Range loop
         Buf (I) := Stream_Element (Val mod 256);
         Val := Val / 256;
      end loop;
      Write (Stream.all, Buf);
   end W_LLI;

   -- Float (simplified - stores as 4 bytes)
   procedure I_F (Stream : not null access Root_Stream_Type'Class;
                  Item   : out Float)
   is
      Buf : Stream_Element_Array (1 .. 4);
      Last : Stream_Element_Offset;
   begin
      Read (Stream.all, Buf, Last);
      -- Note: Proper float deserialization would need more work
      Item := 0.0;
   end I_F;

   procedure I_LF (Stream : not null access Root_Stream_Type'Class;
                   Item   : out Long_Float)
   is
      Buf : Stream_Element_Array (1 .. 6);
      Last : Stream_Element_Offset;
   begin
      Read (Stream.all, Buf, Last);
      Item := 0.0;
   end I_LF;

   procedure W_F (Stream : not null access Root_Stream_Type'Class;
                  Item   : Float)
   is
      pragma Unreferenced (Item);
      Buf : Stream_Element_Array (1 .. 4) := (others => 0);
   begin
      Write (Stream.all, Buf);
   end W_F;

   procedure W_LF (Stream : not null access Root_Stream_Type'Class;
                   Item   : Long_Float)
   is
      pragma Unreferenced (Item);
      Buf : Stream_Element_Array (1 .. 6) := (others => 0);
   begin
      Write (Stream.all, Buf);
   end W_LF;

   -- Character
   procedure I_C (Stream : not null access Root_Stream_Type'Class;
                  Item   : out Character)
   is
      Buf : Stream_Element_Array (1 .. 1);
      Last : Stream_Element_Offset;
   begin
      Read (Stream.all, Buf, Last);
      Item := Character'Val (Natural (Buf (1)));
   end I_C;

   procedure I_WC (Stream : not null access Root_Stream_Type'Class;
                   Item   : out Wide_Character)
   is
      Buf : Stream_Element_Array (1 .. 2);
      Last : Stream_Element_Offset;
   begin
      Read (Stream.all, Buf, Last);
      Item := Wide_Character'Val (Natural (Buf (1)) + Natural (Buf (2)) * 256);
   end I_WC;

   procedure I_WWC (Stream : not null access Root_Stream_Type'Class;
                    Item   : out Wide_Wide_Character)
   is
      Buf : Stream_Element_Array (1 .. 4);
      Last : Stream_Element_Offset;
   begin
      Read (Stream.all, Buf, Last);
      Item := Wide_Wide_Character'Val (
        Natural (Buf (1)) + Natural (Buf (2)) * 256 +
        Natural (Buf (3)) * 65536 + Natural (Buf (4)) * 16777216);
   end I_WWC;

   procedure W_C (Stream : not null access Root_Stream_Type'Class;
                  Item   : Character)
   is
      Buf : Stream_Element_Array (1 .. 1);
   begin
      Buf (1) := Stream_Element (Character'Pos (Item));
      Write (Stream.all, Buf);
   end W_C;

   procedure W_WC (Stream : not null access Root_Stream_Type'Class;
                   Item   : Wide_Character)
   is
      Buf : Stream_Element_Array (1 .. 2);
      Val : constant Natural := Wide_Character'Pos (Item);
   begin
      Buf (1) := Stream_Element (Val mod 256);
      Buf (2) := Stream_Element (Val / 256);
      Write (Stream.all, Buf);
   end W_WC;

   procedure W_WWC (Stream : not null access Root_Stream_Type'Class;
                    Item   : Wide_Wide_Character)
   is
      Buf : Stream_Element_Array (1 .. 4);
      Val : Natural := Wide_Wide_Character'Pos (Item);
   begin
      for I in Buf'Range loop
         Buf (I) := Stream_Element (Val mod 256);
         Val := Val / 256;
      end loop;
      Write (Stream.all, Buf);
   end W_WWC;

   -- Boolean
   procedure I_B (Stream : not null access Root_Stream_Type'Class;
                  Item   : out Boolean)
   is
      Buf : Stream_Element_Array (1 .. 1);
      Last : Stream_Element_Offset;
   begin
      Read (Stream.all, Buf, Last);
      Item := Buf (1) /= 0;
   end I_B;

   procedure W_B (Stream : not null access Root_Stream_Type'Class;
                  Item   : Boolean)
   is
      Buf : Stream_Element_Array (1 .. 1);
   begin
      Buf (1) := (if Item then 1 else 0);
      Write (Stream.all, Buf);
   end W_B;

end System.Stream_Attributes;
