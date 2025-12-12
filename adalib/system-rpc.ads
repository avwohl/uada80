-- System.RPC for Z80
-- Remote Procedure Call support (stub)

with Ada.Streams;

package System.RPC is
   pragma Preelaborate;

   -- Partition communication types
   type Partition_Id is range 0 .. 63;
   -- Z80 typically single partition

   Communication_Error : exception;

   type Params_Stream_Type
     (Initial_Size : Ada.Streams.Stream_Element_Count) is new
       Ada.Streams.Root_Stream_Type with private;

   procedure Read
     (Stream : in out Params_Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Stream : in Out Params_Stream_Type;
      Item   : Ada.Streams.Stream_Element_Array);

   -- RPC operations (stubs for single-partition Z80)
   procedure Do_RPC
     (Partition  : Partition_Id;
      Params     : access Params_Stream_Type;
      Result     : access Params_Stream_Type);

   procedure Do_APC
     (Partition : Partition_Id;
      Params    : access Params_Stream_Type);

   procedure Establish_RPC_Receiver
     (Partition : Partition_Id;
      Receiver  : not null access procedure
                    (Params : access Params_Stream_Type;
                     Result : access Params_Stream_Type));

private

   type Params_Stream_Type
     (Initial_Size : Ada.Streams.Stream_Element_Count) is new
       Ada.Streams.Root_Stream_Type with record
         Buffer : Ada.Streams.Stream_Element_Array (1 .. Initial_Size);
         Count  : Ada.Streams.Stream_Element_Offset := 0;
         Index  : Ada.Streams.Stream_Element_Offset := 1;
       end record;

end System.RPC;
