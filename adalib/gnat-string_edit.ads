-- GNAT.String_Edit for Z80
-- String editing utilities

package GNAT.String_Edit is
   pragma Pure;

   function Get_Aligned
     (Value : String;
      Width : Natural;
      Align : Character := 'L') return String;
   --  Return Value aligned left ('L'), right ('R'), or center ('C')

   function Left_Justify
     (Value : String;
      Width : Natural;
      Pad   : Character := ' ') return String;
   --  Left justify Value in Width characters

   function Right_Justify
     (Value : String;
      Width : Natural;
      Pad   : Character := ' ') return String;
   --  Right justify Value in Width characters

   function Center
     (Value : String;
      Width : Natural;
      Pad   : Character := ' ') return String;
   --  Center Value in Width characters

   function Trim_Left (S : String) return String;
   --  Remove leading blanks

   function Trim_Right (S : String) return String;
   --  Remove trailing blanks

   function Trim (S : String) return String;
   --  Remove leading and trailing blanks

end GNAT.String_Edit;
