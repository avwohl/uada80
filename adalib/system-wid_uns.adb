-- System.Wid_Uns body for Z80
-- Unsigned integer width computation

package body System.Wid_Uns is

   use Interfaces;

   --------------------
   -- Width_Unsigned --
   --------------------

   function Width_Unsigned
     (Lo, Hi : Unsigned_32) return Natural
   is
      Max_Val : Unsigned_32;
      Width   : Natural := 1;
   begin
      if Hi > Lo then
         Max_Val := Hi;
      else
         Max_Val := Lo;
      end if;

      while Max_Val >= 10 loop
         Max_Val := Max_Val / 10;
         Width := Width + 1;
      end loop;

      -- Add 1 for leading space
      return Width + 1;
   end Width_Unsigned;

   -----------------------
   -- Width_Unsigned_16 --
   -----------------------

   function Width_Unsigned_16
     (Lo, Hi : Unsigned_16) return Natural
   is
      Max_Val : Unsigned_16;
      Width   : Natural := 1;
   begin
      if Hi > Lo then
         Max_Val := Hi;
      else
         Max_Val := Lo;
      end if;

      while Max_Val >= 10 loop
         Max_Val := Max_Val / 10;
         Width := Width + 1;
      end loop;

      return Width + 1;
   end Width_Unsigned_16;

   ----------------------
   -- Width_Unsigned_8 --
   ----------------------

   function Width_Unsigned_8
     (Lo, Hi : Unsigned_8) return Natural
   is
      Max_Val : Unsigned_8;
      Width   : Natural := 1;
   begin
      if Hi > Lo then
         Max_Val := Hi;
      else
         Max_Val := Lo;
      end if;

      while Max_Val >= 10 loop
         Max_Val := Max_Val / 10;
         Width := Width + 1;
      end loop;

      return Width + 1;
   end Width_Unsigned_8;

end System.Wid_Uns;
