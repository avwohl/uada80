-- System.Stream_Attributes for Z80
-- Stream attribute support for elementary types

with Ada.Streams;

package System.Stream_Attributes is
   pragma Preelaborate;

   -- Integer stream attributes
   procedure I_I (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                  Item   : out Integer);
   procedure I_SI (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                   Item   : out Short_Integer);
   procedure I_LI (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                   Item   : out Long_Integer);
   procedure I_LLI (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                    Item   : out Long_Long_Integer);

   procedure W_I (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                  Item   : Integer);
   procedure W_SI (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                   Item   : Short_Integer);
   procedure W_LI (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                   Item   : Long_Integer);
   procedure W_LLI (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                    Item   : Long_Long_Integer);

   -- Float stream attributes
   procedure I_F (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                  Item   : out Float);
   procedure I_LF (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                   Item   : out Long_Float);

   procedure W_F (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                  Item   : Float);
   procedure W_LF (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                   Item   : Long_Float);

   -- Character stream attributes
   procedure I_C (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                  Item   : out Character);
   procedure I_WC (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                   Item   : out Wide_Character);
   procedure I_WWC (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                    Item   : out Wide_Wide_Character);

   procedure W_C (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                  Item   : Character);
   procedure W_WC (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                   Item   : Wide_Character);
   procedure W_WWC (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                    Item   : Wide_Wide_Character);

   -- Boolean stream attribute
   procedure I_B (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                  Item   : out Boolean);
   procedure W_B (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                  Item   : Boolean);

end System.Stream_Attributes;
