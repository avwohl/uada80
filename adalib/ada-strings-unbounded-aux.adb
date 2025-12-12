-- Ada.Strings.Unbounded.Aux body for Z80
-- Auxiliary operations implementation

package body Ada.Strings.Unbounded.Aux is

   ----------------
   -- Get_String --
   ----------------

   procedure Get_String
     (U : Unbounded_String;
      S : out String;
      L : out Natural)
   is
      Src : constant String := To_String (U);
   begin
      if Src'Length <= S'Length then
         L := Src'Length;
         S (S'First .. S'First + L - 1) := Src;
      else
         L := S'Length;
         S := Src (Src'First .. Src'First + S'Length - 1);
      end if;
   end Get_String;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String
     (U : in Out Unbounded_String;
      S : String)
   is
   begin
      U := To_Unbounded_String (S);
   end Set_String;

   --------------------
   -- Get_Big_String --
   --------------------

   function Get_Big_String
     (U : Unbounded_String) return String
   is
   begin
      return To_String (U);
   end Get_Big_String;

end Ada.Strings.Unbounded.Aux;
