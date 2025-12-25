-- REPORT.ADA
-- ACATS Report package for UADA80 (Z80/CP/M Ada compiler)
--
-- Simplified implementation that works on Z80/CP/M without Ada.Calendar.
-- Provides the test reporting infrastructure used by all ACATS tests.

package Report is

   subtype File_Num is Integer range 1..5;

   -- The Report routines

   procedure Test
      (Name  : String;
       Descr : String);
   -- Must be called at start of each test

   procedure Failed
      (Descr : String);
   -- Report a test failure

   procedure Not_Applicable
      (Descr : String);
   -- Report test not applicable

   procedure Special_Action
      (Descr : String);
   -- Report special action required

   procedure Comment
      (Descr : String);
   -- Output a comment

   procedure Result;
   -- Must be called at end of each test

   -- Identity functions (ensure values are dynamic, not compile-time)

   function Ident_Int
      (X : Integer) return Integer;

   function Ident_Char
      (X : Character) return Character;

   function Ident_Wide_Char
      (X : Wide_Character) return Wide_Character;

   function Ident_Bool
      (X : Boolean) return Boolean;

   function Ident_Str
      (X : String) return String;

   function Ident_Wide_Str
      (X : Wide_String) return Wide_String;

   function Equal
      (X, Y : Integer) return Boolean;

   -- Utility routines

   function Legal_File_Name
      (X   : File_Num := 1;
       Nam : String := "") return String;

   function Time_Stamp return String;

   -- Event trace setting (disabled for Z80/CP/M)
   Generate_Event_Trace_File : constant Boolean := False;

end Report;


with Ada.Text_IO;
with Version;

package body Report is

   type Status is (Pass, Fail, Does_Not_Apply, Action_Required, Unknown);

   Test_Status : Status := Fail;

   Max_Name_Len : constant := 15;
   Test_Name : String (1..Max_Name_Len);
   Test_Name_Len : Integer range 0..Max_Name_Len := 0;

   No_Name : constant String := "NO_NAME";

   -- Internal helper to output messages
   procedure Put_Msg (Msg : String) is
   begin
      Ada.Text_IO.Put_Line (Msg);
   end Put_Msg;

   procedure Test (Name : String; Descr : String) is
   begin
      Test_Status := Pass;
      if Name'Length <= Max_Name_Len then
         Test_Name_Len := Name'Length;
      else
         Test_Name_Len := Max_Name_Len;
      end if;
      Test_Name (1..Test_Name_Len) :=
         Name (Name'First .. Name'First + Test_Name_Len - 1);

      Put_Msg ("");
      Put_Msg (",.,. " & Test_Name (1..Test_Name_Len) &
               " ACATS " & Version.ACATS_Version);
      Put_Msg ("---- " & Test_Name (1..Test_Name_Len) & " " & Descr & ".");
   end Test;

   procedure Comment (Descr : String) is
   begin
      Put_Msg ("   - " & Test_Name (1..Test_Name_Len) & " " & Descr & ".");
   end Comment;

   procedure Failed (Descr : String) is
   begin
      Test_Status := Fail;
      Put_Msg ("   * " & Test_Name (1..Test_Name_Len) & " " & Descr & ".");
   end Failed;

   procedure Not_Applicable (Descr : String) is
   begin
      if Test_Status = Pass or Test_Status = Action_Required then
         Test_Status := Does_Not_Apply;
      end if;
      Put_Msg ("   + " & Test_Name (1..Test_Name_Len) & " " & Descr & ".");
   end Not_Applicable;

   procedure Special_Action (Descr : String) is
   begin
      if Test_Status = Pass then
         Test_Status := Action_Required;
      end if;
      Put_Msg ("   ! " & Test_Name (1..Test_Name_Len) & " " & Descr & ".");
   end Special_Action;

   procedure Result is
   begin
      case Test_Status is
         when Pass =>
            Put_Msg ("==== " & Test_Name (1..Test_Name_Len) &
                     " PASSED ============================.");
         when Does_Not_Apply =>
            Put_Msg ("++++ " & Test_Name (1..Test_Name_Len) &
                     " NOT-APPLICABLE ++++++++++++++++++++.");
         when Action_Required =>
            Put_Msg ("!!!! " & Test_Name (1..Test_Name_Len) &
                     " TENTATIVELY PASSED !!!!!!!!!!!!!!!!.");
         when others =>
            Put_Msg ("**** " & Test_Name (1..Test_Name_Len) &
                     " FAILED ****************************.");
      end case;
      Test_Status := Fail;
      Test_Name_Len := No_Name'Length;
      Test_Name (1..Test_Name_Len) := No_Name;
   end Result;

   -- Identity functions use Equal to ensure values are dynamic

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

   function Ident_Wide_Char (X : Wide_Character) return Wide_Character is
   begin
      if Equal (Wide_Character'Pos(X), Wide_Character'Pos(X)) then
         return X;
      end if;
      return Wide_Character'Val (48);  -- '0'
   end Ident_Wide_Char;

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

   function Ident_Wide_Str (X : Wide_String) return Wide_String is
      Empty : constant Wide_String (1..0) := (others => Wide_Character'Val (0));
   begin
      if Equal (X'Length, X'Length) then
         return X;
      end if;
      return Empty;
   end Ident_Wide_Str;

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
      (X   : File_Num := 1;
       Nam : String := "") return String
   is
      Suffix : String (2..6);
   begin
      if Nam = "" then
         Suffix := Test_Name (3..7);
      else
         Suffix := Nam (3..7);
      end if;

      case X is
         when 1 => return ('X' & Suffix);
         when 2 => return ('Y' & Suffix);
         when 3 => return ('Z' & Suffix);
         when 4 => return ('V' & Suffix);
         when 5 => return ('W' & Suffix);
      end case;
   end Legal_File_Name;

   function Time_Stamp return String is
   begin
      -- Simplified: no Ada.Calendar on Z80/CP/M
      return "00-00-00 00:00:00";
   end Time_Stamp;

begin
   Test_Name_Len := No_Name'Length;
   Test_Name (1..Test_Name_Len) := No_Name;
end Report;
