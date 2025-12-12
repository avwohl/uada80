-- System.Fore body for Z80
-- Fixed point Fore attribute computation

package body System.Fore is

   ------------------
   -- Fore_Fixed32 --
   ------------------

   function Fore_Fixed32
     (Lo, Hi : Long_Long_Integer;
      Scale  : Integer) return Natural
   is
      Max_Abs  : Long_Long_Integer;
      Int_Part : Long_Long_Integer;
      Result   : Natural := 1;
   begin
      -- Find maximum absolute value
      if abs Lo > abs Hi then
         Max_Abs := abs Lo;
      else
         Max_Abs := abs Hi;
      end if;

      -- Scale to get integer part
      if Scale >= 0 then
         Int_Part := Max_Abs * (2 ** Scale);
      else
         Int_Part := Max_Abs / (2 ** (-Scale));
      end if;

      -- Count digits needed
      while Int_Part >= 10 loop
         Int_Part := Int_Part / 10;
         Result := Result + 1;
      end loop;

      -- Add 1 for sign if range includes negative
      if Lo < 0 then
         Result := Result + 1;
      end if;

      return Result;
   end Fore_Fixed32;

   ------------------
   -- Fore_Fixed64 --
   ------------------

   function Fore_Fixed64
     (Lo, Hi : Long_Long_Integer;
      Scale  : Integer) return Natural
   is
   begin
      return Fore_Fixed32 (Lo, Hi, Scale);
   end Fore_Fixed64;

   ------------------
   -- Generic_Fore --
   ------------------

   function Generic_Fore return Natural is
      Max_Abs : constant T := T'Max (abs T'First, abs T'Last);
      Int_Part : Long_Long_Integer;
      Result   : Natural := 1;
   begin
      Int_Part := Long_Long_Integer (Max_Abs);

      while Int_Part >= 10 loop
         Int_Part := Int_Part / 10;
         Result := Result + 1;
      end loop;

      if T'First < 0.0 then
         Result := Result + 1;
      end if;

      return Result;
   end Generic_Fore;

end System.Fore;
