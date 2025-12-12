-- System.Img_Enum body for Z80
-- Enumeration image implementation

package body System.Img_Enum is

   -----------------------
   -- Image_Enumeration --
   -----------------------

   procedure Image_Enumeration
     (V : Enum;
      S : in Out String;
      P : out Natural)
   is
      Image : constant String := Enum'Image (V);
   begin
      for I in Image'Range loop
         S (S'First + I - Image'First) := Image (I);
      end loop;
      P := S'First + Image'Length - 1;
   end Image_Enumeration;

   -------------------------
   -- Image_Enumeration_8 --
   -------------------------

   procedure Image_Enumeration_8
     (Pos    : Natural;
      Names  : String;
      Starts : String;
      S      : in Out String;
      P      : out Natural)
   is
      Start_Idx : Natural;
      End_Idx   : Natural;
   begin
      if Pos < Starts'Length then
         Start_Idx := Character'Pos (Starts (Starts'First + Pos));

         if Pos + 1 < Starts'Length then
            End_Idx := Character'Pos (Starts (Starts'First + Pos + 1)) - 1;
         else
            End_Idx := Names'Last;
         end if;

         declare
            Len : constant Natural := End_Idx - Start_Idx + 1;
         begin
            for I in 0 .. Len - 1 loop
               S (S'First + I) := Names (Names'First + Start_Idx + I);
            end loop;
            P := S'First + Len - 1;
         end;
      else
         P := S'First - 1;  -- Empty string
      end if;
   end Image_Enumeration_8;

   --------------------------
   -- Image_Enumeration_16 --
   --------------------------

   procedure Image_Enumeration_16
     (Pos    : Natural;
      Names  : String;
      Starts : String;
      S      : in Out String;
      P      : out Natural)
   is
      Start_Idx : Natural;
      End_Idx   : Natural;

      function Get_16 (Idx : Natural) return Natural is
         Lo : constant Natural := Character'Pos (Starts (Starts'First + Idx * 2));
         Hi : constant Natural := Character'Pos (Starts (Starts'First + Idx * 2 + 1));
      begin
         return Lo + Hi * 256;
      end Get_16;

   begin
      if Pos * 2 + 1 < Starts'Length then
         Start_Idx := Get_16 (Pos);

         if (Pos + 1) * 2 + 1 < Starts'Length then
            End_Idx := Get_16 (Pos + 1) - 1;
         else
            End_Idx := Names'Last;
         end if;

         declare
            Len : constant Natural := End_Idx - Start_Idx + 1;
         begin
            for I in 0 .. Len - 1 loop
               S (S'First + I) := Names (Names'First + Start_Idx + I);
            end loop;
            P := S'First + Len - 1;
         end;
      else
         P := S'First - 1;
      end if;
   end Image_Enumeration_16;

end System.Img_Enum;
