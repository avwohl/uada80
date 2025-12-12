-- System.Wid_Util body for Z80
-- Width calculation implementation

package body System.Wid_Util is

   --------------------
   -- Width_Integer --
   --------------------

   function Width_Integer (Lo, Hi : Integer) return Natural is
      W       : Natural := 0;
      Val     : Integer;
      Max_Val : Integer;
   begin
      -- Width of Lo
      Val := Lo;
      if Val < 0 then
         W := 1;  -- For minus sign
         Val := -Val;
      end if;

      if Val = 0 then
         W := W + 1;
      else
         while Val > 0 loop
            W := W + 1;
            Val := Val / 10;
         end loop;
      end if;

      -- Width of Hi
      Max_Val := Hi;
      declare
         W2 : Natural := 0;
      begin
         if Max_Val < 0 then
            W2 := 1;
            Max_Val := -Max_Val;
         end if;

         if Max_Val = 0 then
            W2 := W2 + 1;
         else
            while Max_Val > 0 loop
               W2 := W2 + 1;
               Max_Val := Max_Val / 10;
            end loop;
         end if;

         if W2 > W then
            W := W2;
         end if;
      end;

      return W;
   end Width_Integer;

   ---------------------
   -- Width_Unsigned --
   ---------------------

   function Width_Unsigned (Lo, Hi : Natural) return Natural is
      W   : Natural := 0;
      Val : Natural;
   begin
      -- Width of Hi (larger value)
      Val := Hi;

      if Val = 0 then
         W := 1;
      else
         while Val > 0 loop
            W := W + 1;
            Val := Val / 10;
         end loop;
      end if;

      return W;
   end Width_Unsigned;

   -----------------------
   -- Width_Enumeration --
   -----------------------

   function Width_Enumeration
     (Lo, Hi     : Integer;
      Width_Func : access function (Index : Integer) return Natural)
      return Natural
   is
      W     : Natural := 0;
      Cur_W : Natural;
   begin
      for I in Lo .. Hi loop
         Cur_W := Width_Func (I);
         if Cur_W > W then
            W := Cur_W;
         end if;
      end loop;
      return W;
   end Width_Enumeration;

end System.Wid_Util;
