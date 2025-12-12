-- GNAT.Expect body for Z80
-- Process control and expect-style interaction (stub implementation)

package body GNAT.Expect is

   Not_Supported : constant String := "Process control not supported on Z80/CP-M";

   ------------------------
   -- Non_Blocking_Spawn --
   ------------------------

   procedure Non_Blocking_Spawn
     (Descriptor  : out Process_Descriptor;
      Command     : String;
      Args        : GNAT.OS_Lib.Argument_List;
      Buffer_Size : Natural := 4096)
   is
      pragma Unreferenced (Command, Args, Buffer_Size);
   begin
      Descriptor.PID := -1;
      Descriptor.Buffer_Len := 0;
      raise Invalid_Process with Not_Supported;
   end Non_Blocking_Spawn;

   -----------
   -- Close --
   -----------

   procedure Close (Descriptor : in Out Process_Descriptor) is
   begin
      Descriptor.PID := -1;
      Descriptor.Buffer_Len := 0;
   end Close;

   ------------
   -- Expect --
   ------------

   procedure Expect
     (Descriptor  : in Out Process_Descriptor;
      Result      : out Natural;
      Regexp      : String;
      Timeout     : Integer := 10000)
   is
      pragma Unreferenced (Regexp, Timeout);
   begin
      if Descriptor.PID < 0 then
         raise Invalid_Process;
      end if;
      Result := 0;
      raise Process_Died with Not_Supported;
   end Expect;

   ----------
   -- Send --
   ----------

   procedure Send
     (Descriptor : in Out Process_Descriptor;
      Str        : String)
   is
      pragma Unreferenced (Str);
   begin
      if Descriptor.PID < 0 then
         raise Invalid_Process;
      end if;
      raise Invalid_Process with Not_Supported;
   end Send;

   ----------------
   -- Expect_Out --
   ----------------

   function Expect_Out (Descriptor : Process_Descriptor) return String is
   begin
      return Descriptor.Buffer (1 .. Descriptor.Buffer_Len);
   end Expect_Out;

end GNAT.Expect;
