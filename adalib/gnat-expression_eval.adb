-- GNAT.Expression_Eval body for Z80
-- Expression evaluator implementation using recursive descent

package body GNAT.Expression_Eval is

   -- Parsing state
   Current_Pos : Natural;
   Current_Expr : String (1 .. Max_Expr_Length);
   Expr_Length : Natural;
   Parse_Error : Boolean;
   Error_Pos   : Natural;
   Use_Vars    : Boolean := False;

   procedure Skip_Spaces is
   begin
      while Current_Pos <= Expr_Length and then
            Current_Expr (Current_Pos) = ' ' loop
         Current_Pos := Current_Pos + 1;
      end loop;
   end Skip_Spaces;

   function Current_Char return Character is
   begin
      if Current_Pos <= Expr_Length then
         return Current_Expr (Current_Pos);
      else
         return Character'Val (0);
      end if;
   end Current_Char;

   procedure Advance is
   begin
      Current_Pos := Current_Pos + 1;
   end Advance;

   --------------
   -- Is_Digit --
   --------------

   function Is_Digit (C : Character) return Boolean is
   begin
      return C >= '0' and C <= '9';
   end Is_Digit;

   -----------------
   -- Is_Operator --
   -----------------

   function Is_Operator (C : Character) return Boolean is
   begin
      return C = '+' or C = '-' or C = '*' or C = '/' or C = '%';
   end Is_Operator;

   -----------------
   -- Is_Variable --
   -----------------

   function Is_Variable (C : Character) return Boolean is
   begin
      return C >= 'A' and C <= 'Z';
   end Is_Variable;

   -- Forward declarations for recursive descent
   function Parse_Expression return Integer;
   function Parse_Term return Integer;
   function Parse_Factor return Integer;

   ------------------
   -- Parse_Number --
   ------------------

   function Parse_Number return Integer is
      Value    : Integer := 0;
      Negative : Boolean := False;
   begin
      Skip_Spaces;

      if Current_Char = '-' then
         Negative := True;
         Advance;
         Skip_Spaces;
      elsif Current_Char = '+' then
         Advance;
         Skip_Spaces;
      end if;

      if not Is_Digit (Current_Char) then
         Parse_Error := True;
         Error_Pos := Current_Pos;
         return 0;
      end if;

      while Is_Digit (Current_Char) loop
         Value := Value * 10 + (Character'Pos (Current_Char) - Character'Pos ('0'));
         Advance;
      end loop;

      if Negative then
         Value := -Value;
      end if;

      return Value;
   end Parse_Number;

   ------------------
   -- Parse_Factor --
   ------------------

   function Parse_Factor return Integer is
      Value : Integer;
   begin
      Skip_Spaces;

      if Current_Char = '(' then
         Advance;
         Value := Parse_Expression;
         Skip_Spaces;
         if Current_Char = ')' then
            Advance;
         else
            Parse_Error := True;
            Error_Pos := Current_Pos;
         end if;
         return Value;

      elsif Use_Vars and Is_Variable (Current_Char) then
         Value := Variables (Current_Char);
         Advance;
         return Value;

      elsif Current_Char = '-' then
         Advance;
         return -Parse_Factor;

      elsif Current_Char = '+' then
         Advance;
         return Parse_Factor;

      else
         return Parse_Number;
      end if;
   end Parse_Factor;

   ----------------
   -- Parse_Term --
   ----------------

   function Parse_Term return Integer is
      Left  : Integer;
      Right : Integer;
      Op    : Character;
   begin
      Left := Parse_Factor;

      loop
         Skip_Spaces;
         Op := Current_Char;
         exit when Op /= '*' and Op /= '/' and Op /= '%';

         Advance;
         Right := Parse_Factor;

         if Parse_Error then
            return 0;
         end if;

         case Op is
            when '*' =>
               Left := Left * Right;
            when '/' =>
               if Right = 0 then
                  Parse_Error := True;
                  Error_Pos := Current_Pos;
                  return 0;
               end if;
               Left := Left / Right;
            when '%' =>
               if Right = 0 then
                  Parse_Error := True;
                  Error_Pos := Current_Pos;
                  return 0;
               end if;
               Left := Left mod Right;
            when others =>
               null;
         end case;
      end loop;

      return Left;
   end Parse_Term;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression return Integer is
      Left  : Integer;
      Right : Integer;
      Op    : Character;
   begin
      Left := Parse_Term;

      loop
         Skip_Spaces;
         Op := Current_Char;
         exit when Op /= '+' and Op /= '-';

         Advance;
         Right := Parse_Term;

         if Parse_Error then
            return 0;
         end if;

         case Op is
            when '+' =>
               Left := Left + Right;
            when '-' =>
               Left := Left - Right;
            when others =>
               null;
         end case;
      end loop;

      return Left;
   end Parse_Expression;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Expr : String) return Eval_Result is
      Result : Eval_Result;
   begin
      -- Initialize parsing state
      Current_Expr := (others => ' ');
      Expr_Length := Expr'Length;
      if Expr_Length > Max_Expr_Length then
         Expr_Length := Max_Expr_Length;
      end if;

      for I in 1 .. Expr_Length loop
         Current_Expr (I) := Expr (Expr'First + I - 1);
      end loop;

      Current_Pos := 1;
      Parse_Error := False;
      Error_Pos := 0;
      Use_Vars := False;

      Result.Value := Parse_Expression;
      Skip_Spaces;

      -- Check for trailing garbage
      if Current_Pos <= Expr_Length then
         Parse_Error := True;
         Error_Pos := Current_Pos;
      end if;

      Result.Success := not Parse_Error;
      Result.Error := Error_Pos;

      return Result;
   end Evaluate;

   ----------
   -- Eval --
   ----------

   function Eval (Expr : String) return Integer is
      R : constant Eval_Result := Evaluate (Expr);
   begin
      if R.Success then
         return R.Value;
      else
         return 0;
      end if;
   end Eval;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Expr : String) return Boolean is
      R : constant Eval_Result := Evaluate (Expr);
   begin
      return R.Success;
   end Is_Valid;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (Pos : Natural) return String is
      Pos_Str : constant String := Natural'Image (Pos);
   begin
      if Pos = 0 then
         return "No error";
      else
         return "Error at position" & Pos_Str;
      end if;
   end Error_Message;

   ------------------
   -- Set_Variable --
   ------------------

   procedure Set_Variable (Name : Character; Value : Integer) is
   begin
      if Name >= 'A' and Name <= 'Z' then
         Variables (Name) := Value;
      end if;
   end Set_Variable;

   ------------------
   -- Get_Variable --
   ------------------

   function Get_Variable (Name : Character) return Integer is
   begin
      if Name >= 'A' and Name <= 'Z' then
         return Variables (Name);
      else
         return 0;
      end if;
   end Get_Variable;

   ---------------------
   -- Clear_Variables --
   ---------------------

   procedure Clear_Variables is
   begin
      Variables := (others => 0);
   end Clear_Variables;

   ------------------------
   -- Evaluate_With_Vars --
   ------------------------

   function Evaluate_With_Vars (Expr : String) return Eval_Result is
      Result : Eval_Result;
   begin
      -- Initialize parsing state
      Current_Expr := (others => ' ');
      Expr_Length := Expr'Length;
      if Expr_Length > Max_Expr_Length then
         Expr_Length := Max_Expr_Length;
      end if;

      for I in 1 .. Expr_Length loop
         Current_Expr (I) := Expr (Expr'First + I - 1);
      end loop;

      Current_Pos := 1;
      Parse_Error := False;
      Error_Pos := 0;
      Use_Vars := True;

      Result.Value := Parse_Expression;
      Skip_Spaces;

      -- Check for trailing garbage
      if Current_Pos <= Expr_Length then
         Parse_Error := True;
         Error_Pos := Current_Pos;
      end if;

      Result.Success := not Parse_Error;
      Result.Error := Error_Pos;

      return Result;
   end Evaluate_With_Vars;

end GNAT.Expression_Eval;
