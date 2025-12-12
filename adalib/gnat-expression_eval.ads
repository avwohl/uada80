-- GNAT.Expression_Eval for Z80
-- Simple arithmetic expression evaluator

package GNAT.Expression_Eval is
   pragma Preelaborate;

   Max_Expr_Length : constant := 64;
   Max_Stack_Depth : constant := 16;

   -- Evaluation result
   type Eval_Result is record
      Value   : Integer;
      Success : Boolean;
      Error   : Natural;  -- Position of error, 0 if success
   end record;

   -- Evaluate integer arithmetic expression
   -- Supports: + - * / % ( ) and integer literals
   -- Spaces are ignored
   function Evaluate (Expr : String) return Eval_Result;

   -- Simple evaluate returning just the value (0 on error)
   function Eval (Expr : String) return Integer;

   -- Check if expression is valid
   function Is_Valid (Expr : String) return Boolean;

   -- Get error message for failed evaluation
   function Error_Message (Pos : Natural) return String;

   -- Variable support (up to 26 single-letter variables A-Z)
   procedure Set_Variable (Name : Character; Value : Integer);
   function Get_Variable (Name : Character) return Integer;
   procedure Clear_Variables;

   -- Evaluate with variables
   -- Variables are single uppercase letters A-Z
   function Evaluate_With_Vars (Expr : String) return Eval_Result;

   -- Helper to check if character is valid in expression
   function Is_Digit (C : Character) return Boolean;
   function Is_Operator (C : Character) return Boolean;
   function Is_Variable (C : Character) return Boolean;

private

   Variables : array (Character range 'A' .. 'Z') of Integer :=
     (others => 0);

end GNAT.Expression_Eval;
