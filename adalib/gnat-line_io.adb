-- GNAT.Line_IO body for Z80
-- Line-oriented I/O implementation

with GNAT.IO;

package body GNAT.Line_IO is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Buffer : out Line_Buffer) is
   begin
      Buffer.Length := 0;
   end Initialize;

   ---------------
   -- Read_Line --
   ---------------

   procedure Read_Line
     (Buffer : in Out Line_Buffer;
      S      : out String;
      Last   : out Natural)
   is
   begin
      GNAT.IO.Get_Line (Buffer.Data, Buffer.Length);
      if Buffer.Length > S'Length then
         S := Buffer.Data (1 .. S'Length);
         Last := S'Last;
      else
         S (S'First .. S'First + Buffer.Length - 1) :=
           Buffer.Data (1 .. Buffer.Length);
         Last := S'First + Buffer.Length - 1;
      end if;
   end Read_Line;

   ----------------
   -- Write_Line --
   ----------------

   procedure Write_Line (S : String) is
   begin
      GNAT.IO.Put_Line (S);
   end Write_Line;

   ----------------
   -- Write_Line --
   ----------------

   procedure Write_Line is
   begin
      GNAT.IO.New_Line;
   end Write_Line;

   --------------
   -- Get_Line --
   --------------

   function Get_Line return String is
      Buffer : String (1 .. Max_Line_Length);
      Last   : Natural;
   begin
      GNAT.IO.Get_Line (Buffer, Last);
      return Buffer (1 .. Last);
   end Get_Line;

end GNAT.Line_IO;
