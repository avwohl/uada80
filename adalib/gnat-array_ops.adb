-- GNAT.Array_Ops body for Z80
-- Generic array operations implementation

package body GNAT.Array_Ops is

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : Array_Type;
      Item      : Element_Type) return Boolean
   is
   begin
      for E of Container loop
         if E = Item then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   -----------
   -- Index --
   -----------

   function Index
     (Container : Array_Type;
      Item      : Element_Type) return Index_Type'Base
   is
   begin
      for I in Container'Range loop
         if Container (I) = Item then
            return I;
         end if;
      end loop;
      return Index_Type'First - 1;
   end Index;

   -----------
   -- Count --
   -----------

   function Count
     (Container : Array_Type;
      Item      : Element_Type) return Natural
   is
      Result : Natural := 0;
   begin
      for E of Container loop
         if E = Item then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count;

   -------------------
   -- Reverse_Array --
   -------------------

   procedure Reverse_Array (Container : in Out Array_Type) is
      Temp : Element_Type;
      J    : Index_Type := Container'Last;
   begin
      for I in Container'First .. Index_Type'Val (
         (Index_Type'Pos (Container'First) + Index_Type'Pos (Container'Last)) / 2)
      loop
         Temp := Container (I);
         Container (I) := Container (J);
         Container (J) := Temp;
         J := Index_Type'Pred (J);
      end loop;
   end Reverse_Array;

   -----------------
   -- Rotate_Left --
   -----------------

   procedure Rotate_Left (Container : in Out Array_Type; Amount : Natural := 1) is
      Temp : Element_Type;
      Len  : constant Natural := Container'Length;
      Amt  : constant Natural := Amount mod Len;
   begin
      if Len = 0 or Amt = 0 then
         return;
      end if;

      for Count in 1 .. Amt loop
         Temp := Container (Container'First);
         for I in Container'First .. Index_Type'Pred (Container'Last) loop
            Container (I) := Container (Index_Type'Succ (I));
         end loop;
         Container (Container'Last) := Temp;
      end loop;
   end Rotate_Left;

   ------------------
   -- Rotate_Right --
   ------------------

   procedure Rotate_Right (Container : in Out Array_Type; Amount : Natural := 1) is
      Temp : Element_Type;
      Len  : constant Natural := Container'Length;
      Amt  : constant Natural := Amount mod Len;
   begin
      if Len = 0 or Amt = 0 then
         return;
      end if;

      for Count in 1 .. Amt loop
         Temp := Container (Container'Last);
         for I in reverse Index_Type'Succ (Container'First) .. Container'Last loop
            Container (I) := Container (Index_Type'Pred (I));
         end loop;
         Container (Container'First) := Temp;
      end loop;
   end Rotate_Right;

end GNAT.Array_Ops;
