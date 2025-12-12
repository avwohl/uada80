-- GNAT.Memory_Dump body for Z80
-- Memory dump implementation

with Ada.Text_IO;
with System.Storage_Elements;

package body GNAT.Memory_Dump is

   use System.Storage_Elements;

   Hex_Digits : constant String := "0123456789ABCDEF";

   function Hex_Byte (B : Storage_Element) return String is
      Result : String (1 .. 2);
   begin
      Result (1) := Hex_Digits (Natural (B / 16) + 1);
      Result (2) := Hex_Digits (Natural (B mod 16) + 1);
      return Result;
   end Hex_Byte;

   function Hex_Word (W : Integer_Address) return String is
      Result : String (1 .. 4);
      V      : Integer_Address := W;
   begin
      for I in reverse 1 .. 4 loop
         Result (I) := Hex_Digits (Natural (V mod 16) + 1);
         V := V / 16;
      end loop;
      return Result;
   end Hex_Word;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Addr   : System.Address;
      Count  : Natural)
   is
   begin
      Dump (Addr, Count, "");
   end Dump;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Addr   : System.Address;
      Count  : Natural;
      Prefix : String)
   is
      Mem : Storage_Array (0 .. Storage_Offset (Count - 1));
      for Mem'Address use Addr;

      Line_Bytes : constant := 16;
      Current    : Storage_Offset := 0;
      Line_Addr  : Integer_Address;
      B          : Storage_Element;
      Ascii_Part : String (1 .. Line_Bytes);
   begin
      while Current < Storage_Offset (Count) loop
         Line_Addr := To_Integer (Addr) + Integer_Address (Current);

         -- Print prefix and address
         if Prefix'Length > 0 then
            Ada.Text_IO.Put (Prefix);
            Ada.Text_IO.Put (" ");
         end if;
         Ada.Text_IO.Put (Hex_Word (Line_Addr));
         Ada.Text_IO.Put (": ");

         -- Print hex bytes and build ASCII representation
         Ascii_Part := (others => '.');
         for I in 0 .. Line_Bytes - 1 loop
            if Current + Storage_Offset (I) < Storage_Offset (Count) then
               B := Mem (Current + Storage_Offset (I));
               Ada.Text_IO.Put (Hex_Byte (B));
               Ada.Text_IO.Put (" ");

               -- ASCII representation
               if B >= 32 and B < 127 then
                  Ascii_Part (I + 1) := Character'Val (B);
               end if;
            else
               Ada.Text_IO.Put ("   ");
            end if;
         end loop;

         -- Print ASCII part
         Ada.Text_IO.Put (" |");
         Ada.Text_IO.Put (Ascii_Part);
         Ada.Text_IO.Put_Line ("|");

         Current := Current + Line_Bytes;
      end loop;
   end Dump;

end GNAT.Memory_Dump;
