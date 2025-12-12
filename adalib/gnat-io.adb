-- GNAT.IO body for Z80
-- Simple I/O routines implementation

with Ada.Text_IO;

package body GNAT.IO is

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      Ada.Text_IO.Put (C);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String) is
   begin
      Ada.Text_IO.Put_Line (S);
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Ada.Text_IO.New_Line;
   end New_Line;

   ---------
   -- Get --
   ---------

   function Get return Character is
      C : Character;
   begin
      Ada.Text_IO.Get (C);
      return C;
   end Get;

   procedure Get (C : out Character) is
   begin
      Ada.Text_IO.Get (C);
   end Get;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (S : out String; Last : out Natural) is
   begin
      Ada.Text_IO.Get_Line (S, Last);
   end Get_Line;

end GNAT.IO;
