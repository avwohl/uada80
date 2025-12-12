-- System.Wid_Enum body for Z80
-- Enumeration width computation implementation

package body System.Wid_Enum is

   -----------------------
   -- Width_Enumeration --
   -----------------------

   function Width_Enumeration (Lo, Hi : Enum) return Natural is
      Max_Width : Natural := 0;
   begin
      for E in Lo .. Hi loop
         declare
            Image : constant String := Enum'Image (E);
         begin
            if Image'Length > Max_Width then
               Max_Width := Image'Length;
            end if;
         end;
      end loop;
      return Max_Width;
   end Width_Enumeration;

   -------------------------
   -- Width_Enumeration_8 --
   -------------------------

   function Width_Enumeration_8
     (Names  : String;
      Starts : String;
      Lo, Hi : Natural) return Natural
   is
      Max_Width : Natural := 0;
      Start_Idx : Natural;
      End_Idx   : Natural;
      Len       : Natural;
   begin
      for Pos in Lo .. Hi loop
         if Pos < Starts'Length then
            Start_Idx := Character'Pos (Starts (Starts'First + Pos));

            if Pos + 1 < Starts'Length then
               End_Idx := Character'Pos (Starts (Starts'First + Pos + 1)) - 1;
            else
               End_Idx := Names'Last - Names'First;
            end if;

            Len := End_Idx - Start_Idx + 1;
            if Len > Max_Width then
               Max_Width := Len;
            end if;
         end if;
      end loop;
      return Max_Width;
   end Width_Enumeration_8;

   --------------------------
   -- Width_Enumeration_16 --
   --------------------------

   function Width_Enumeration_16
     (Names  : String;
      Starts : String;
      Lo, Hi : Natural) return Natural
   is
      Max_Width : Natural := 0;
      Start_Idx : Natural;
      End_Idx   : Natural;
      Len       : Natural;

      function Get_16 (Idx : Natural) return Natural is
         Base : constant Natural := Starts'First + Idx * 2;
         Lo_Byte : constant Natural := Character'Pos (Starts (Base));
         Hi_Byte : constant Natural := Character'Pos (Starts (Base + 1));
      begin
         return Lo_Byte + Hi_Byte * 256;
      end Get_16;

   begin
      for Pos in Lo .. Hi loop
         if Pos * 2 + 1 < Starts'Length then
            Start_Idx := Get_16 (Pos);

            if (Pos + 1) * 2 + 1 < Starts'Length then
               End_Idx := Get_16 (Pos + 1) - 1;
            else
               End_Idx := Names'Last - Names'First;
            end if;

            Len := End_Idx - Start_Idx + 1;
            if Len > Max_Width then
               Max_Width := Len;
            end if;
         end if;
      end loop;
      return Max_Width;
   end Width_Enumeration_16;

end System.Wid_Enum;
