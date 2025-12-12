-- Ada.Command_Line.Response_File body for Z80
-- Response file processing implementation

with Ada.Text_IO;

package body Ada.Command_Line.Response_File is

   File      : Ada.Text_IO.File_Type;
   File_Open : Boolean := False;
   Buffer    : String (1 .. 256);
   Buf_Len   : Natural := 0;
   Buf_Pos   : Natural := 0;

   ----------
   -- Open --
   ----------

   procedure Open (Name : String) is
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Name);
      File_Open := True;
      Buf_Len := 0;
      Buf_Pos := 0;
   exception
      when others =>
         File_Open := False;
   end Open;

   -------------
   -- Is_Open --
   -------------

   function Is_Open return Boolean is
   begin
      return File_Open;
   end Is_Open;

   --------------
   -- Argument --
   --------------

   function Argument return String is
      Start, Finish : Natural;
   begin
      if not File_Open then
         return "";
      end if;

      -- Read more if needed
      if Buf_Pos >= Buf_Len then
         if Ada.Text_IO.End_Of_File (File) then
            return "";
         end if;
         Ada.Text_IO.Get_Line (File, Buffer, Buf_Len);
         Buf_Pos := 0;
      end if;

      -- Skip whitespace
      while Buf_Pos < Buf_Len and then
            (Buffer (Buf_Pos + 1) = ' ' or Buffer (Buf_Pos + 1) = ASCII.HT)
      loop
         Buf_Pos := Buf_Pos + 1;
      end loop;

      if Buf_Pos >= Buf_Len then
         return "";
      end if;

      Start := Buf_Pos + 1;

      -- Find end of argument
      while Buf_Pos < Buf_Len and then
            Buffer (Buf_Pos + 1) /= ' ' and then
            Buffer (Buf_Pos + 1) /= ASCII.HT
      loop
         Buf_Pos := Buf_Pos + 1;
      end loop;

      Finish := Buf_Pos;
      return Buffer (Start .. Finish);
   end Argument;

   --------------------
   -- More_Arguments --
   --------------------

   function More_Arguments return Boolean is
   begin
      if not File_Open then
         return False;
      end if;
      return not Ada.Text_IO.End_Of_File (File) or Buf_Pos < Buf_Len;
   end More_Arguments;

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      if File_Open then
         Ada.Text_IO.Close (File);
         File_Open := False;
      end if;
   end Close;

end Ada.Command_Line.Response_File;
