-- Z80/CP/M implementation of ACATS Report package
-- Simplified to avoid dynamic string slicing (codegen limitation)

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
     function Ident_Bool (X : Boolean) return Boolean;
     function Ident_Str (X : String) return String;

     function Equal (X, Y : Integer) return Boolean;

     function Legal_File_Name
        (X : File_Num := 1;
         Nam : String := "")
        return String;

     function Time_Stamp return String;

     Generate_Event_Trace_File : constant Boolean := False;

end Report;

with Ada.Text_IO;
use Ada.Text_IO;

package body Report is

     type Status is (Pass, Fail, Does_Not_Apply, Action_Required,
                     Unknown);

     Test_Status : Status := Fail;
     Have_Name : Boolean := False;

     procedure Test (Name : String; Descr : String) is
     begin
          Test_Status := Pass;
          Have_Name := True;
          Put_Line (",.,. " & Name & " ACATS 4.2A");
          Put_Line ("---- " & Name & " " & Descr & ".");
     end Test;

     procedure Comment (Descr : String) is
     begin
          Put_Line ("   - " & Descr & ".");
     end Comment;

     procedure Failed (Descr : String) is
     begin
          Test_Status := Fail;
          Put_Line ("   * " & Descr & ".");
     end Failed;

     procedure Not_Applicable (Descr : String) is
     begin
          if Test_Status = Pass or Test_Status = Action_Required then
               Test_Status := Does_Not_Apply;
          end if;
          Put_Line ("   + " & Descr & ".");
     end Not_Applicable;

     procedure Special_Action (Descr : String) is
     begin
          if Test_Status = Pass then
               Test_Status := Action_Required;
          end if;
          Put_Line ("   ! " & Descr & ".");
     end Special_Action;

     procedure Result is
     begin
          case Test_Status is
          when Pass =>
               Put_Line ("==== PASSED ============================.");
          when Does_Not_Apply =>
               Put_Line ("++++ NOT-APPLICABLE ++++++++++++++++++++.");
          when Action_Required =>
               Put_Line ("!!!! TENTATIVELY PASSED !!!!!!!!!!!!!!!!.");
          when others =>
               Put_Line ("**** FAILED ****************************.");
          end case;
          Test_Status := Fail;
     end Result;

     function Ident_Int (X : Integer) return Integer is
     begin
          if Equal (X, X) then
               return X;
          end if;
          return 0;
     end Ident_Int;

     function Ident_Char (X : Character) return Character is
     begin
          if Equal (Character'Pos(X), Character'Pos(X)) then
               return X;
          end if;
          return '0';
     end Ident_Char;

     function Ident_Bool (X : Boolean) return Boolean is
     begin
          if Equal (Boolean'Pos(X), Boolean'Pos(X)) then
               return X;
          end if;
          return False;
     end Ident_Bool;

     function Ident_Str (X : String) return String is
     begin
          if Equal (X'Length, X'Length) then
               return X;
          end if;
          return "";
     end Ident_Str;

     function Equal (X, Y : Integer) return Boolean is
          Rec_Limit : constant Integer range 1..100 := 3;
          Z : Boolean;
     begin
          if X < 0 then
               if Y < 0 then
                    Z := Equal (-X, -Y);
               else
                    Z := False;
               end if;
          elsif X > Rec_Limit then
               Z := Equal (Rec_Limit, Y - X + Rec_Limit);
          elsif X > 0 then
               Z := Equal (X - 1, Y - 1);
          else
               Z := Y = 0;
          end if;
          return Z;
     exception
          when others =>
               return X = Y;
     end Equal;

     function Legal_File_Name
        (X : File_Num := 1;
         Nam : String := "")
        return String is
     begin
          case X is
               when 1 => return "X00000";
               when 2 => return "Y00000";
               when 3 => return "Z00000";
               when 4 => return "V00000";
               when 5 => return "W00000";
          end case;
     end Legal_File_Name;

     function Time_Stamp return String is
     begin
          return "00-01-01 00:00:00";
     end Time_Stamp;

end Report;
