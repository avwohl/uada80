-- GNAT.Casing_Utilities body for Z80
-- Case manipulation implementation

package body GNAT.Casing_Utilities is

   --------------
   -- Is_Upper --
   --------------

   function Is_Upper (C : Character) return Boolean is
   begin
      return C in 'A' .. 'Z';
   end Is_Upper;

   --------------
   -- Is_Lower --
   --------------

   function Is_Lower (C : Character) return Boolean is
   begin
      return C in 'a' .. 'z';
   end Is_Lower;

   --------------
   -- To_Upper --
   --------------

   function To_Upper (C : Character) return Character is
   begin
      if Is_Lower (C) then
         return Character'Val (Character'Pos (C) - 32);
      else
         return C;
      end if;
   end To_Upper;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (C : Character) return Character is
   begin
      if Is_Upper (C) then
         return Character'Val (Character'Pos (C) + 32);
      else
         return C;
      end if;
   end To_Lower;

   ----------------------
   -- Determine_Casing --
   ----------------------

   function Determine_Casing (Name : String) return Casing_Type is
      Has_Upper : Boolean := False;
      Has_Lower : Boolean := False;
   begin
      for C of Name loop
         if Is_Upper (C) then
            Has_Upper := True;
         elsif Is_Lower (C) then
            Has_Lower := True;
         end if;
      end loop;

      if Has_Upper and Has_Lower then
         return Mixed_Case;
      elsif Has_Upper then
         return All_Upper_Case;
      elsif Has_Lower then
         return All_Lower_Case;
      else
         return Unknown;
      end if;
   end Determine_Casing;

   ----------------
   -- Set_Casing --
   ----------------

   procedure Set_Casing (Name : in Out String; Casing : Casing_Type) is
   begin
      case Casing is
         when All_Upper_Case =>
            Set_All_Upper_Case (Name);
         when All_Lower_Case =>
            Set_All_Lower_Case (Name);
         when Mixed_Case =>
            Set_Mixed_Case (Name);
         when Unknown =>
            null;
      end case;
   end Set_Casing;

   ------------------------
   -- Set_All_Upper_Case --
   ------------------------

   procedure Set_All_Upper_Case (Name : in Out String) is
   begin
      for I in Name'Range loop
         Name (I) := To_Upper (Name (I));
      end loop;
   end Set_All_Upper_Case;

   ------------------------
   -- Set_All_Lower_Case --
   ------------------------

   procedure Set_All_Lower_Case (Name : in Out String) is
   begin
      for I in Name'Range loop
         Name (I) := To_Lower (Name (I));
      end loop;
   end Set_All_Lower_Case;

   --------------------
   -- Set_Mixed_Case --
   --------------------

   procedure Set_Mixed_Case (Name : in Out String) is
      After_Underscore : Boolean := True;
   begin
      for I in Name'Range loop
         if Name (I) = '_' then
            After_Underscore := True;
         elsif After_Underscore then
            Name (I) := To_Upper (Name (I));
            After_Underscore := False;
         else
            Name (I) := To_Lower (Name (I));
         end if;
      end loop;
   end Set_Mixed_Case;

end GNAT.Casing_Utilities;
