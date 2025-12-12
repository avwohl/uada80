-- System.IO body for Z80/CP/M
-- Low-level I/O implementation using BDOS

with System.CRTL;

package body System.IO is

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      System.CRTL.BDOS_Console_Output (Character'Pos (C));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (S : String) is
   begin
      for C of S loop
         Put (C);
      end loop;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String) is
   begin
      Put (S);
      New_Line;
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Put (ASCII.CR);
      Put (ASCII.LF);
   end New_Line;

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      return Character'Val (System.CRTL.BDOS_Console_Input);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get (C : out Character) is
   begin
      C := Get;
   end Get;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (S : out String; Last : out Natural) is
      C : Character;
   begin
      Last := S'First - 1;

      loop
         C := Get;

         exit when C = ASCII.CR or C = ASCII.LF;

         if Last < S'Last then
            Last := Last + 1;
            S (Last) := C;
         end if;
      end loop;

      -- Skip trailing LF if CR was received
      if C = ASCII.CR and then Character_Available then
         C := Get_Immediate;
         if C /= ASCII.LF then
            -- Put it back somehow (not possible on CP/M, ignore)
            null;
         end if;
      end if;
   end Get_Line;

   -------------------
   -- Get_Immediate --
   -------------------

   function Get_Immediate return Character is
   begin
      -- BDOS function 6 with input FF
      return Character'Val (System.CRTL.BDOS_Direct_Console_IO (16#FF#));
   end Get_Immediate;

   -------------------------
   -- Character_Available --
   -------------------------

   function Character_Available return Boolean is
   begin
      -- BDOS function 11 - Console status
      return System.CRTL.BDOS_Console_Status /= 0;
   end Character_Available;

end System.IO;
