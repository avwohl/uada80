-- GNAT.Rewrite_Data for Z80
-- Data rewriting utilities

package GNAT.Rewrite_Data is
   pragma Preelaborate;

   type Buffer is private;

   procedure Create
     (B           : out Buffer;
      Pattern     : String;
      Replacement : String);
   --  Create a rewrite buffer

   procedure Rewrite (B : in Out Buffer; Input : String; Output : out String; Last : out Natural);
   --  Apply rewriting rules to input

   procedure Flush (B : in Out Buffer; Output : out String; Last : out Natural);
   --  Flush any pending data

   procedure Reset (B : in Out Buffer);
   --  Reset buffer state

private
   Max_Pattern : constant := 64;

   type Buffer is record
      Pattern     : String (1 .. Max_Pattern);
      Pat_Len     : Natural := 0;
      Replacement : String (1 .. Max_Pattern);
      Rep_Len     : Natural := 0;
      Pending     : String (1 .. Max_Pattern);
      Pend_Len    : Natural := 0;
   end record;

end GNAT.Rewrite_Data;
