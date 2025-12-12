-- System.RPC body for Z80
-- Remote Procedure Call support (stub implementation)

package body System.RPC is

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in Out Params_Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      use Ada.Streams;
   begin
      for I in Item'Range loop
         exit when Stream.Index > Stream.Count;
         Item (I) := Stream.Buffer (Stream.Index);
         Stream.Index := Stream.Index + 1;
         Last := I;
      end loop;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in Out Params_Stream_Type;
      Item   : Ada.Streams.Stream_Element_Array)
   is
      use Ada.Streams;
   begin
      for I in Item'Range loop
         Stream.Count := Stream.Count + 1;
         if Stream.Count <= Stream.Buffer'Last then
            Stream.Buffer (Stream.Count) := Item (I);
         end if;
      end loop;
   end Write;

   ------------
   -- Do_RPC --
   ------------

   procedure Do_RPC
     (Partition  : Partition_Id;
      Params     : access Params_Stream_Type;
      Result     : access Params_Stream_Type)
   is
      pragma Unreferenced (Partition, Params, Result);
   begin
      -- Z80 is single partition, no remote calls
      raise Communication_Error with "Remote partitions not supported on Z80";
   end Do_RPC;

   ------------
   -- Do_APC --
   ------------

   procedure Do_APC
     (Partition : Partition_Id;
      Params    : access Params_Stream_Type)
   is
      pragma Unreferenced (Partition, Params);
   begin
      -- Z80 is single partition, no remote calls
      raise Communication_Error with "Remote partitions not supported on Z80";
   end Do_APC;

   ----------------------------
   -- Establish_RPC_Receiver --
   ----------------------------

   procedure Establish_RPC_Receiver
     (Partition : Partition_Id;
      Receiver  : not null access procedure
                    (Params : access Params_Stream_Type;
                     Result : access Params_Stream_Type))
   is
      pragma Unreferenced (Partition, Receiver);
   begin
      -- No-op on single partition system
      null;
   end Establish_RPC_Receiver;

end System.RPC;
