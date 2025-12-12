-- System.IO for Z80/CP/M
-- Low-level I/O primitives

package System.IO is
   pragma Preelaborate;

   -- Character I/O
   procedure Put (C : Character);
   procedure Put (S : String);
   procedure Put_Line (S : String);
   procedure New_Line;

   function Get return Character;
   procedure Get (C : out Character);
   procedure Get_Line (S : out String; Last : out Natural);

   -- Direct console I/O (no echo)
   function Get_Immediate return Character;
   function Character_Available return Boolean;

end System.IO;
