-- GNAT.String_Edit body for Z80
-- String editing utilities implementation

package body GNAT.String_Edit is

   -----------------
   -- Get_Aligned --
   -----------------

   function Get_Aligned
     (Value : String;
      Width : Natural;
      Align : Character := 'L') return String
   is
   begin
      case Align is
         when 'L' | 'l' => return Left_Justify (Value, Width);
         when 'R' | 'r' => return Right_Justify (Value, Width);
         when 'C' | 'c' => return Center (Value, Width);
         when others    => return Left_Justify (Value, Width);
      end case;
   end Get_Aligned;

   ------------------
   -- Left_Justify --
   ------------------

   function Left_Justify
     (Value : String;
      Width : Natural;
      Pad   : Character := ' ') return String
   is
   begin
      if Value'Length >= Width then
         return Value (Value'First .. Value'First + Width - 1);
      else
         declare
            Result : String (1 .. Width) := (others => Pad);
         begin
            Result (1 .. Value'Length) := Value;
            return Result;
         end;
      end if;
   end Left_Justify;

   -------------------
   -- Right_Justify --
   -------------------

   function Right_Justify
     (Value : String;
      Width : Natural;
      Pad   : Character := ' ') return String
   is
   begin
      if Value'Length >= Width then
         return Value (Value'Last - Width + 1 .. Value'Last);
      else
         declare
            Result : String (1 .. Width) := (others => Pad);
            Start  : constant Positive := Width - Value'Length + 1;
         begin
            Result (Start .. Width) := Value;
            return Result;
         end;
      end if;
   end Right_Justify;

   ------------
   -- Center --
   ------------

   function Center
     (Value : String;
      Width : Natural;
      Pad   : Character := ' ') return String
   is
   begin
      if Value'Length >= Width then
         return Value (Value'First .. Value'First + Width - 1);
      else
         declare
            Result : String (1 .. Width) := (others => Pad);
            Start  : constant Positive := (Width - Value'Length) / 2 + 1;
         begin
            Result (Start .. Start + Value'Length - 1) := Value;
            return Result;
         end;
      end if;
   end Center;

   ---------------
   -- Trim_Left --
   ---------------

   function Trim_Left (S : String) return String is
      First : Natural := S'First;
   begin
      while First <= S'Last and then S (First) = ' ' loop
         First := First + 1;
      end loop;
      return S (First .. S'Last);
   end Trim_Left;

   ----------------
   -- Trim_Right --
   ----------------

   function Trim_Right (S : String) return String is
      Last : Natural := S'Last;
   begin
      while Last >= S'First and then S (Last) = ' ' loop
         Last := Last - 1;
      end loop;
      return S (S'First .. Last);
   end Trim_Right;

   ----------
   -- Trim --
   ----------

   function Trim (S : String) return String is
   begin
      return Trim_Right (Trim_Left (S));
   end Trim;

end GNAT.String_Edit;
