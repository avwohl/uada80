-- Report package body for ACATS test harness
-- Z80/CP/M implementation using direct console I/O

with Ada.Text_IO;

package body Report is

   Test_Status : Integer := 0;  -- 0=passed, 1=failed, 2=NA
   Test_Name_Store : String(1..20) := (others => ' ');
   Test_Name_Len : Integer := 0;

   procedure Test (Name : String; Descr : String) is
   begin
      Test_Status := 0;
      -- Store test name
      Test_Name_Len := Integer'Min(Name'Length, 20);
      for I in 1 .. Test_Name_Len loop
         Test_Name_Store(I) := Name(Name'First + I - 1);
      end loop;

      Ada.Text_IO.Put_Line(",.,. " & Name & " ACATS 4.2");
      Ada.Text_IO.Put_Line("---- " & Name & " " & Descr & ".");
   end Test;

   procedure Failed (Descr : String) is
   begin
      Test_Status := 1;
      Ada.Text_IO.Put_Line("   * " & Descr & ".");
   end Failed;

   procedure Not_Applicable (Descr : String) is
   begin
      if Test_Status = 0 then
         Test_Status := 2;
      end if;
      Ada.Text_IO.Put_Line("   + " & Descr & ".");
   end Not_Applicable;

   procedure Special_Action (Descr : String) is
   begin
      Ada.Text_IO.Put_Line("   ! " & Descr & ".");
   end Special_Action;

   procedure Comment (Descr : String) is
   begin
      Ada.Text_IO.Put_Line("   - " & Descr & ".");
   end Comment;

   procedure Result is
   begin
      case Test_Status is
         when 1 =>
            Ada.Text_IO.Put_Line("**** FAILED ****");
         when 2 =>
            Ada.Text_IO.Put_Line("++++ NOT APPLICABLE ++++");
         when others =>
            Ada.Text_IO.Put_Line("==== PASSED ====");
      end case;
   end Result;

   function Ident_Int (X : Integer) return Integer is
   begin
      return X;
   end Ident_Int;

   function Ident_Char (X : Character) return Character is
   begin
      return X;
   end Ident_Char;

   function Ident_Wide_Char (X : Wide_Character) return Wide_Character is
   begin
      return X;
   end Ident_Wide_Char;

   function Ident_Bool (X : Boolean) return Boolean is
   begin
      return X;
   end Ident_Bool;

   function Ident_Str (X : String) return String is
   begin
      return X;
   end Ident_Str;

   function Ident_Wide_Str (X : Wide_String) return Wide_String is
   begin
      return X;
   end Ident_Wide_Str;

   function Equal (X, Y : Integer) return Boolean is
   begin
      return X = Y;
   end Equal;

   function Legal_File_Name (X : File_Num := 1; Nam : String := "") return String is
   begin
      case X is
         when 1 => return "X2100A";
         when 2 => return "X2100B";
         when 3 => return "X3100A";
         when 4 => return "X3100B";
         when 5 => return "X4100A";
      end case;
   end Legal_File_Name;

   function Time_Stamp return String is
   begin
      return "00-00-00 00:00:00";
   end Time_Stamp;

end Report;
