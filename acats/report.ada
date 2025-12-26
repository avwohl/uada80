-- REPORT.ADA
-- ACATS Report package for UADA80 (Z80/CP/M Ada compiler)
-- Simplified implementation that works on Z80/CP/M

package Report is
   subtype File_Num is Integer range 1..5;

   procedure Test (Name : String; Descr : String);
   procedure Failed (Descr : String);
   procedure Not_Applicable (Descr : String);
   procedure Special_Action (Descr : String);
   procedure Comment (Descr : String);
   procedure Result;

   function Ident_Int (X : Integer) return Integer;
   function Ident_Char (X : Character) return Character;
   function Ident_Wide_Char (X : Wide_Character) return Wide_Character;
   function Ident_Bool (X : Boolean) return Boolean;
   function Ident_Str (X : String) return String;
   function Ident_Wide_Str (X : Wide_String) return Wide_String;
   function Equal (X, Y : Integer) return Boolean;
   function Legal_File_Name (X : File_Num := 1; Nam : String := "") return String;
   function Time_Stamp return String;

   Generate_Event_Trace_File : constant Boolean := False;
end Report;

with Ada.Text_IO;

package body Report is
   Test_Failed : Boolean := False;
   Test_NA : Boolean := False;
   Current_Name : String(1..15) := "               ";
   Name_Len : Integer := 0;

   procedure Test (Name : String; Descr : String) is
   begin
      Test_Failed := False;
      Test_NA := False;
      -- Store name
      if Name'Length <= 15 then
         Name_Len := Name'Length;
      else
         Name_Len := 15;
      end if;
      for I in 1..Name_Len loop
         Current_Name(I) := Name(Name'First + I - 1);
      end loop;

      Ada.Text_IO.Put_Line(",.,. " & Name & " ACATS 4.2");
      Ada.Text_IO.Put_Line("---- " & Name & " " & Descr & ".");
   end Test;

   procedure Comment (Descr : String) is
   begin
      Ada.Text_IO.Put_Line("   - " & Descr & ".");
   end Comment;

   procedure Failed (Descr : String) is
   begin
      Test_Failed := True;
      Ada.Text_IO.Put_Line("   * " & Descr & ".");
   end Failed;

   procedure Not_Applicable (Descr : String) is
   begin
      if not Test_Failed then
         Test_NA := True;
      end if;
      Ada.Text_IO.Put_Line("   + " & Descr & ".");
   end Not_Applicable;

   procedure Special_Action (Descr : String) is
   begin
      Ada.Text_IO.Put_Line("   ! " & Descr & ".");
   end Special_Action;

   procedure Result is
   begin
      if Test_Failed then
         Ada.Text_IO.Put_Line("**** FAILED ****");
      elsif Test_NA then
         Ada.Text_IO.Put_Line("++++ NOT-APPLICABLE ++++");
      else
         Ada.Text_IO.Put_Line("==== PASSED ====");
      end if;
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
         when 1 => return "X00001";
         when 2 => return "Y00002";
         when 3 => return "Z00003";
         when 4 => return "V00004";
         when 5 => return "W00005";
      end case;
   end Legal_File_Name;

   function Time_Stamp return String is
   begin
      return "00-00-00 00:00:00";
   end Time_Stamp;

end Report;
