-- System.Traceback body for Z80
-- Stack traceback support

package body System.Traceback is

   ---------------------
   -- Get_Call_Chain --
   ---------------------

   procedure Get_Call_Chain
     (Chain : out Call_Chain;
      Max_Depth : Natural := Max_Traceback_Depth)
   is
      pragma Unreferenced (Max_Depth);
   begin
      -- Z80 doesn't have standard frame pointers
      -- Return empty traceback
      Chain.Addresses := Null_Traceback;
      Chain.Length := 0;

      -- In a real implementation, we might:
      -- 1. Walk the stack looking for return addresses
      -- 2. Use debug info if available
      -- For now, just return empty
   end Get_Call_Chain;

   ------------------------
   -- Symbolic_Traceback --
   ------------------------

   function Symbolic_Traceback (Address : System.Address) return String is
      Hex : String (1 .. 4);
      Val : Natural := Natural (Address);
      D   : Natural;
   begin
      -- Convert address to hex string
      for I in reverse Hex'Range loop
         D := Val mod 16;
         if D < 10 then
            Hex (I) := Character'Val (Character'Pos ('0') + D);
         else
            Hex (I) := Character'Val (Character'Pos ('A') + D - 10);
         end if;
         Val := Val / 16;
      end loop;

      return "0x" & Hex;
   end Symbolic_Traceback;

   ----------------------
   -- Format_Traceback --
   ----------------------

   function Format_Traceback (Chain : Call_Chain) return String is
   begin
      if Chain.Length = 0 then
         return "(no traceback available)";
      end if;

      -- Build traceback string
      declare
         Result : String (1 .. Chain.Length * 10);
         Pos    : Natural := 1;
      begin
         for I in 1 .. Chain.Length loop
            declare
               Addr : constant String := Symbolic_Traceback (Chain.Addresses (I));
            begin
               for J in Addr'Range loop
                  if Pos <= Result'Last then
                     Result (Pos) := Addr (J);
                     Pos := Pos + 1;
                  end if;
               end loop;

               if I < Chain.Length and then Pos < Result'Last then
                  Result (Pos) := ' ';
                  Pos := Pos + 1;
               end if;
            end;
         end loop;

         return Result (1 .. Pos - 1);
      end;
   end Format_Traceback;

end System.Traceback;
