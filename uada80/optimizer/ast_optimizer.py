"""
AST Optimizer for Ada.

Performs source-level optimizations on the AST before lowering to IR.
Based on proven optimization strategies from uplm80.

Optimization Levels:
- Level 0: No optimizations
- Level 1: Constant folding, algebraic simplifications
- Level 2: + Strength reduction, dead code elimination, boolean simplifications
- Level 3: + CSE, copy propagation, loop optimizations
"""

from dataclasses import replace
from typing import Any, Optional

from uada80.ast_nodes import (
    # Base
    Expr,
    Stmt,
    Decl,
    # Expressions
    Identifier,
    IntegerLiteral,
    RealLiteral,
    StringLiteral,
    CharacterLiteral,
    NullLiteral,
    BinaryExpr,
    UnaryExpr,
    BinaryOp,
    UnaryOp,
    Parenthesized,
    AttributeReference,
    IndexedComponent,
    FunctionCall,
    RangeExpr,
    TypeConversion,
    QualifiedExpr,
    ConditionalExpr,
    # Choices for case statements
    Choice,
    ExprChoice,
    RangeChoice,
    OthersChoice,
    # Statements
    NullStmt,
    AssignmentStmt,
    IfStmt,
    CaseStmt,
    LoopStmt,
    WhileScheme,
    ForScheme,
    BlockStmt,
    ExitStmt,
    ReturnStmt,
    ProcedureCallStmt,
    # Declarations
    ObjectDecl,
    SubprogramBody,
    PackageBody,
    # Top-level
    Program,
    CompilationUnit,
)
from uada80.optimizer.config import OptimizerConfig, OptimizationStats, OptimizationLevel


class ASTOptimizer:
    """
    AST-level optimizer for Ada.

    Transforms the AST to improve code quality while preserving semantics.
    """

    def __init__(self, config: Optional[OptimizerConfig] = None):
        self.config = config or OptimizerConfig()
        self.stats = OptimizationStats()
        self._changed = False  # Track if any optimization was applied
        self._constant_values: dict[str, Any] = {}  # Known constant values
        self._cse_cache: dict[str, Expr] = {}  # CSE expression cache

    def optimize(self, program: Program) -> Program:
        """
        Optimize an entire program.

        Runs multiple passes until no more optimizations can be applied.
        """
        if self.config.level == OptimizationLevel.NONE:
            return program

        # Multi-pass optimization
        max_passes = 10
        for pass_num in range(max_passes):
            self._changed = False
            program = self._optimize_program(program)
            if not self._changed:
                break

        return program

    def _optimize_program(self, program: Program) -> Program:
        """Optimize all compilation units in a program."""
        optimized_units = [self._optimize_unit(unit) for unit in program.units]
        return replace(program, units=optimized_units)

    def _optimize_unit(self, unit: CompilationUnit) -> CompilationUnit:
        """Optimize a compilation unit."""
        optimized = self._optimize_decl(unit.unit)
        return replace(unit, unit=optimized)

    # =========================================================================
    # Declaration Optimization
    # =========================================================================

    def _optimize_decl(self, decl: Decl) -> Decl:
        """Optimize a declaration."""
        if isinstance(decl, SubprogramBody):
            return self._optimize_subprogram_body(decl)
        elif isinstance(decl, PackageBody):
            return self._optimize_package_body(decl)
        elif isinstance(decl, ObjectDecl):
            return self._optimize_object_decl(decl)
        return decl

    def _optimize_subprogram_body(self, body: SubprogramBody) -> SubprogramBody:
        """Optimize a subprogram body."""
        # Clear per-subprogram state
        self._constant_values.clear()
        self._cse_cache.clear()

        # Collect known constants from declarations
        for decl in body.declarations:
            if isinstance(decl, ObjectDecl) and decl.is_constant and decl.init_expr:
                val = self._try_evaluate_constant(decl.init_expr)
                if val is not None:
                    for name in decl.names:
                        self._constant_values[name] = val

        # Optimize declarations
        opt_decls = [self._optimize_decl(d) for d in body.declarations]

        # Optimize statements
        opt_stmts = self._optimize_statement_list(body.statements)

        return replace(body, declarations=opt_decls, statements=opt_stmts)

    def _optimize_package_body(self, body: PackageBody) -> PackageBody:
        """Optimize a package body."""
        opt_decls = [self._optimize_decl(d) for d in body.declarations]
        opt_stmts = self._optimize_statement_list(body.statements)
        return replace(body, declarations=opt_decls, statements=opt_stmts)

    def _optimize_object_decl(self, decl: ObjectDecl) -> ObjectDecl:
        """Optimize an object declaration (mainly its initializer)."""
        if decl.init_expr:
            opt_init = self._optimize_expr(decl.init_expr)
            return replace(decl, init_expr=opt_init)
        return decl

    # =========================================================================
    # Statement Optimization
    # =========================================================================

    def _optimize_statement_list(self, stmts: list[Stmt]) -> list[Stmt]:
        """Optimize a list of statements."""
        result = []
        for stmt in stmts:
            opt_stmt = self._optimize_stmt(stmt)
            if opt_stmt is not None:
                # Dead code elimination: skip statements after unconditional exit/return
                result.append(opt_stmt)
                if self.config.eliminate_dead_code:
                    if isinstance(opt_stmt, ReturnStmt):
                        # Everything after return is dead
                        self._changed = True
                        self.stats.dead_code_eliminated += len(stmts) - len(result)
                        break
        return result

    def _optimize_stmt(self, stmt: Stmt) -> Optional[Stmt]:
        """Optimize a single statement. Returns None if statement should be removed."""
        if isinstance(stmt, AssignmentStmt):
            return self._optimize_assignment(stmt)
        elif isinstance(stmt, IfStmt):
            return self._optimize_if(stmt)
        elif isinstance(stmt, LoopStmt):
            return self._optimize_loop(stmt)
        elif isinstance(stmt, CaseStmt):
            return self._optimize_case(stmt)
        elif isinstance(stmt, BlockStmt):
            return self._optimize_block(stmt)
        elif isinstance(stmt, ProcedureCallStmt):
            return self._optimize_procedure_call(stmt)
        elif isinstance(stmt, ReturnStmt):
            return self._optimize_return(stmt)
        elif isinstance(stmt, ExitStmt):
            return self._optimize_exit(stmt)
        elif isinstance(stmt, NullStmt):
            return stmt
        return stmt

    def _optimize_assignment(self, stmt: AssignmentStmt) -> AssignmentStmt:
        """Optimize an assignment statement."""
        opt_target = self._optimize_expr(stmt.target)
        opt_value = self._optimize_expr(stmt.value)
        return replace(stmt, target=opt_target, value=opt_value)

    def _optimize_if(self, stmt: IfStmt) -> Optional[Stmt]:
        """Optimize an if statement."""
        opt_condition = self._optimize_expr(stmt.condition)

        # Try to evaluate condition at compile time
        if self.config.eliminate_dead_code:
            cond_val = self._try_evaluate_boolean(opt_condition)
            if cond_val is True:
                # Condition always true - replace with then branch
                self._changed = True
                self.stats.dead_code_eliminated += 1
                opt_stmts = self._optimize_statement_list(stmt.then_stmts)
                if len(opt_stmts) == 1:
                    return opt_stmts[0]
                return BlockStmt(statements=opt_stmts)
            elif cond_val is False:
                # Condition always false - use else branch or elsif
                self._changed = True
                self.stats.dead_code_eliminated += 1
                if stmt.elsif_parts:
                    # Convert first elsif to if
                    first_elsif = stmt.elsif_parts[0]
                    new_if = IfStmt(
                        condition=first_elsif[0],
                        then_stmts=first_elsif[1],
                        elsif_parts=stmt.elsif_parts[1:],
                        else_stmts=stmt.else_stmts,
                    )
                    return self._optimize_if(new_if)
                elif stmt.else_stmts:
                    opt_stmts = self._optimize_statement_list(stmt.else_stmts)
                    if len(opt_stmts) == 1:
                        return opt_stmts[0]
                    return BlockStmt(statements=opt_stmts)
                else:
                    # No else, no elsif - remove entire if
                    return NullStmt()

        # Optimize branches
        opt_then = self._optimize_statement_list(stmt.then_stmts)
        opt_elsif = [
            (self._optimize_expr(cond), self._optimize_statement_list(stmts))
            for cond, stmts in stmt.elsif_parts
        ]
        opt_else = self._optimize_statement_list(stmt.else_stmts)

        return replace(
            stmt,
            condition=opt_condition,
            then_stmts=opt_then,
            elsif_parts=opt_elsif,
            else_stmts=opt_else,
        )

    def _optimize_loop(self, stmt: LoopStmt) -> Optional[Stmt]:
        """Optimize a loop statement."""
        opt_stmts = self._optimize_statement_list(stmt.statements)

        if stmt.iteration_scheme:
            if isinstance(stmt.iteration_scheme, WhileScheme):
                opt_cond = self._optimize_expr(stmt.iteration_scheme.condition)

                # Check for while False - loop never executes
                if self.config.eliminate_dead_code:
                    cond_val = self._try_evaluate_boolean(opt_cond)
                    if cond_val is False:
                        self._changed = True
                        self.stats.dead_code_eliminated += 1
                        return NullStmt()

                opt_scheme = replace(stmt.iteration_scheme, condition=opt_cond)
                return replace(stmt, iteration_scheme=opt_scheme, statements=opt_stmts)

            elif isinstance(stmt.iteration_scheme, ForScheme):
                # Optimize the range expression in for loops
                iterator = stmt.iteration_scheme.iterator
                if iterator.iterable:
                    opt_iterable = self._optimize_expr(iterator.iterable)
                    opt_iterator = replace(iterator, iterable=opt_iterable)
                    opt_scheme = replace(stmt.iteration_scheme, iterator=opt_iterator)
                    return replace(stmt, iteration_scheme=opt_scheme, statements=opt_stmts)

        return replace(stmt, statements=opt_stmts)

    def _optimize_case(self, stmt: CaseStmt) -> Stmt:
        """Optimize a case statement.

        If the selector expression is a compile-time constant, we can
        eliminate the entire case statement and keep only the matching
        alternative's statements wrapped in a block.
        """
        opt_expr = self._optimize_expr(stmt.expr)

        # Check if selector is a constant we can evaluate
        const_val = self._get_constant_value(opt_expr)

        if const_val is not None:
            # Find the matching alternative
            for alt in stmt.alternatives:
                if self._choice_matches(alt.choices, const_val):
                    # Optimize the matching statements and return as block
                    opt_stmts = self._optimize_statement_list(alt.statements)
                    self._changed = True
                    self.stats.cases_eliminated += 1
                    # Return a block containing just the matching statements
                    return BlockStmt(
                        declarations=[],
                        statements=opt_stmts,
                        label=None,
                        span=stmt.span
                    )

        # Otherwise optimize alternatives normally
        opt_alternatives = []
        for alt in stmt.alternatives:
            opt_stmts = self._optimize_statement_list(alt.statements)
            opt_alternatives.append(replace(alt, statements=opt_stmts))

        return replace(stmt, expr=opt_expr, alternatives=opt_alternatives)

    def _choice_matches(self, choices: list[Choice], value: Any) -> bool:
        """Check if a constant value matches any of the given choices."""
        for choice in choices:
            if isinstance(choice, OthersChoice):
                # 'others' always matches as fallback
                return True
            elif isinstance(choice, ExprChoice):
                choice_val = self._get_constant_value(choice.expr)
                if choice_val is not None and choice_val == value:
                    return True
            elif isinstance(choice, RangeChoice):
                low_val = self._get_constant_value(choice.range_expr.low)
                high_val = self._get_constant_value(choice.range_expr.high)
                if low_val is not None and high_val is not None:
                    if low_val <= value <= high_val:
                        return True
        return False

    def _optimize_block(self, stmt: BlockStmt) -> BlockStmt:
        """Optimize a block statement."""
        opt_decls = [self._optimize_decl(d) for d in stmt.declarations]
        opt_stmts = self._optimize_statement_list(stmt.statements)
        return replace(stmt, declarations=opt_decls, statements=opt_stmts)

    def _optimize_procedure_call(self, stmt: ProcedureCallStmt) -> ProcedureCallStmt:
        """Optimize a procedure call statement."""
        opt_args = []
        for arg in stmt.args:
            if arg.value:
                opt_value = self._optimize_expr(arg.value)
                opt_args.append(replace(arg, value=opt_value))
            else:
                opt_args.append(arg)
        return replace(stmt, args=opt_args)

    def _optimize_return(self, stmt: ReturnStmt) -> ReturnStmt:
        """Optimize a return statement."""
        if stmt.value:
            opt_value = self._optimize_expr(stmt.value)
            return replace(stmt, value=opt_value)
        return stmt

    def _optimize_exit(self, stmt: ExitStmt) -> ExitStmt:
        """Optimize an exit statement."""
        if stmt.condition:
            opt_cond = self._optimize_expr(stmt.condition)
            return replace(stmt, condition=opt_cond)
        return stmt

    # =========================================================================
    # Expression Optimization
    # =========================================================================

    def _optimize_expr(self, expr: Expr) -> Expr:
        """Optimize an expression."""
        if isinstance(expr, BinaryExpr):
            return self._optimize_binary(expr)
        elif isinstance(expr, UnaryExpr):
            return self._optimize_unary(expr)
        elif isinstance(expr, Parenthesized):
            return self._optimize_parenthesized(expr)
        elif isinstance(expr, Identifier):
            return self._optimize_identifier(expr)
        elif isinstance(expr, AttributeReference):
            return self._optimize_attribute(expr)
        elif isinstance(expr, IndexedComponent):
            return self._optimize_indexed(expr)
        elif isinstance(expr, FunctionCall):
            return self._optimize_function_call(expr)
        elif isinstance(expr, RangeExpr):
            return self._optimize_range(expr)
        elif isinstance(expr, ConditionalExpr):
            return self._optimize_conditional_expr(expr)
        elif isinstance(expr, TypeConversion):
            return self._optimize_type_conversion(expr)
        elif isinstance(expr, (IntegerLiteral, RealLiteral, StringLiteral,
                               CharacterLiteral, NullLiteral)):
            return expr
        return expr

    def _optimize_binary(self, expr: BinaryExpr) -> Expr:
        """Optimize a binary expression."""
        opt_left = self._optimize_expr(expr.left)
        opt_right = self._optimize_expr(expr.right)

        # Constant folding
        if self.config.fold_constants:
            result = self._try_fold_binary(expr.op, opt_left, opt_right)
            if result is not None:
                self._changed = True
                self.stats.constants_folded += 1
                return result

        # Algebraic simplifications
        if self.config.algebraic_simplify:
            result = self._algebraic_simplify(expr.op, opt_left, opt_right)
            if result is not None:
                self._changed = True
                self.stats.algebraic_simplifications += 1
                return result

        # Strength reduction
        if self.config.strength_reduce:
            result = self._strength_reduce(expr.op, opt_left, opt_right)
            if result is not None:
                self._changed = True
                self.stats.strength_reductions += 1
                return result

        # Boolean simplifications
        if self.config.boolean_simplify:
            result = self._boolean_simplify(expr.op, opt_left, opt_right)
            if result is not None:
                self._changed = True
                self.stats.boolean_simplifications += 1
                return result

        return replace(expr, left=opt_left, right=opt_right)

    def _optimize_unary(self, expr: UnaryExpr) -> Expr:
        """Optimize a unary expression."""
        opt_operand = self._optimize_expr(expr.operand)

        # Constant folding
        if self.config.fold_constants:
            result = self._try_fold_unary(expr.op, opt_operand)
            if result is not None:
                self._changed = True
                self.stats.constants_folded += 1
                return result

        # Double negation: --x = x
        if self.config.algebraic_simplify:
            inner = self._unwrap_parens(opt_operand)
            if expr.op == UnaryOp.MINUS and isinstance(inner, UnaryExpr):
                if inner.op == UnaryOp.MINUS:
                    self._changed = True
                    self.stats.algebraic_simplifications += 1
                    return inner.operand

            # not not x = x
            if expr.op == UnaryOp.NOT and isinstance(inner, UnaryExpr):
                if inner.op == UnaryOp.NOT:
                    self._changed = True
                    self.stats.algebraic_simplifications += 1
                    return inner.operand

        return replace(expr, operand=opt_operand)

    def _optimize_parenthesized(self, expr: Parenthesized) -> Expr:
        """Optimize a parenthesized expression."""
        opt_inner = self._optimize_expr(expr.expr)

        # Remove unnecessary parentheses around literals and identifiers
        if isinstance(opt_inner, (IntegerLiteral, RealLiteral, Identifier)):
            return opt_inner

        return replace(expr, expr=opt_inner)

    def _optimize_identifier(self, expr: Identifier) -> Expr:
        """Optimize an identifier (propagate constants)."""
        if self.config.propagate_copies and expr.name in self._constant_values:
            val = self._constant_values[expr.name]
            if isinstance(val, int):
                self._changed = True
                self.stats.copies_propagated += 1
                return IntegerLiteral(value=val, text=str(val))
            elif isinstance(val, bool):
                self._changed = True
                self.stats.copies_propagated += 1
                return Identifier(name="True" if val else "False")
        return expr

    def _optimize_attribute(self, expr: AttributeReference) -> Expr:
        """Optimize attribute references (Ada-specific)."""
        opt_prefix = self._optimize_expr(expr.prefix)

        if self.config.evaluate_attributes:
            # Try to evaluate static attributes
            result = self._try_evaluate_attribute(opt_prefix, expr.attribute, expr.args)
            if result is not None:
                self._changed = True
                self.stats.attribute_evaluations += 1
                return result

        opt_args = [self._optimize_expr(arg) for arg in expr.args]
        return replace(expr, prefix=opt_prefix, args=opt_args)

    def _optimize_indexed(self, expr: IndexedComponent) -> IndexedComponent:
        """Optimize indexed component (array access)."""
        opt_prefix = self._optimize_expr(expr.prefix)
        opt_indices = [self._optimize_expr(idx) for idx in expr.indices]
        return replace(expr, prefix=opt_prefix, indices=opt_indices)

    def _optimize_function_call(self, expr: FunctionCall) -> FunctionCall:
        """Optimize a function call."""
        opt_args = []
        for arg in expr.args:
            if arg.value:
                opt_value = self._optimize_expr(arg.value)
                opt_args.append(replace(arg, value=opt_value))
            else:
                opt_args.append(arg)
        return replace(expr, args=opt_args)

    def _optimize_range(self, expr: RangeExpr) -> RangeExpr:
        """Optimize a range expression."""
        opt_low = self._optimize_expr(expr.low)
        opt_high = self._optimize_expr(expr.high)
        return replace(expr, low=opt_low, high=opt_high)

    def _optimize_conditional_expr(self, expr: ConditionalExpr) -> Expr:
        """Optimize a conditional expression (Ada 2012)."""
        opt_cond = self._optimize_expr(expr.condition)

        # Constant condition - pick appropriate branch
        if self.config.eliminate_dead_code:
            cond_val = self._try_evaluate_boolean(opt_cond)
            if cond_val is True:
                self._changed = True
                self.stats.dead_code_eliminated += 1
                return self._optimize_expr(expr.then_expr)
            elif cond_val is False:
                self._changed = True
                self.stats.dead_code_eliminated += 1
                if expr.elsif_parts:
                    # Handle elsif chain
                    first_cond, first_expr = expr.elsif_parts[0]
                    new_expr = ConditionalExpr(
                        condition=first_cond,
                        then_expr=first_expr,
                        elsif_parts=expr.elsif_parts[1:],
                        else_expr=expr.else_expr,
                    )
                    return self._optimize_conditional_expr(new_expr)
                elif expr.else_expr:
                    return self._optimize_expr(expr.else_expr)

        opt_then = self._optimize_expr(expr.then_expr)
        opt_elsif = [
            (self._optimize_expr(c), self._optimize_expr(e))
            for c, e in expr.elsif_parts
        ]
        opt_else = self._optimize_expr(expr.else_expr) if expr.else_expr else None

        return replace(
            expr,
            condition=opt_cond,
            then_expr=opt_then,
            elsif_parts=opt_elsif,
            else_expr=opt_else,
        )

    def _optimize_type_conversion(self, expr: TypeConversion) -> Expr:
        """Optimize type conversion."""
        opt_operand = self._optimize_expr(expr.operand)
        return replace(expr, operand=opt_operand)

    # =========================================================================
    # Constant Folding
    # =========================================================================

    def _try_fold_binary(self, op: BinaryOp, left: Expr, right: Expr) -> Optional[Expr]:
        """Try to fold a binary expression with constant operands."""
        left_val = self._get_integer_value(left)
        right_val = self._get_integer_value(right)

        if left_val is not None and right_val is not None:
            try:
                result = self._eval_binary_int(op, left_val, right_val)
                if result is not None:
                    if isinstance(result, bool):
                        return Identifier(name="True" if result else "False")
                    return IntegerLiteral(value=result, text=str(result))
            except (ZeroDivisionError, OverflowError):
                return None

        return None

    def _try_fold_unary(self, op: UnaryOp, operand: Expr) -> Optional[Expr]:
        """Try to fold a unary expression with constant operand."""
        val = self._get_integer_value(operand)

        if val is not None:
            if op == UnaryOp.MINUS:
                return IntegerLiteral(value=-val, text=str(-val))
            elif op == UnaryOp.PLUS:
                return IntegerLiteral(value=val, text=str(val))
            elif op == UnaryOp.ABS:
                return IntegerLiteral(value=abs(val), text=str(abs(val)))

        # Boolean NOT
        if op == UnaryOp.NOT:
            bool_val = self._try_evaluate_boolean(operand)
            if bool_val is not None:
                return Identifier(name="True" if not bool_val else "False")

        return None

    def _eval_binary_int(self, op: BinaryOp, left: int, right: int) -> Optional[int | bool]:
        """Evaluate a binary integer operation."""
        if op == BinaryOp.ADD:
            return left + right
        elif op == BinaryOp.SUB:
            return left - right
        elif op == BinaryOp.MUL:
            return left * right
        elif op == BinaryOp.DIV:
            if right == 0:
                return None
            return left // right
        elif op == BinaryOp.MOD:
            if right == 0:
                return None
            return left % right
        elif op == BinaryOp.REM:
            if right == 0:
                return None
            # Ada REM follows sign of dividend
            result = abs(left) % abs(right)
            return result if left >= 0 else -result
        elif op == BinaryOp.EXP:
            if right < 0:
                return None
            return left ** right
        elif op == BinaryOp.EQ:
            return left == right
        elif op == BinaryOp.NE:
            return left != right
        elif op == BinaryOp.LT:
            return left < right
        elif op == BinaryOp.LE:
            return left <= right
        elif op == BinaryOp.GT:
            return left > right
        elif op == BinaryOp.GE:
            return left >= right
        return None

    # =========================================================================
    # Algebraic Simplifications
    # =========================================================================

    def _algebraic_simplify(self, op: BinaryOp, left: Expr, right: Expr) -> Optional[Expr]:
        """Apply algebraic simplification rules."""
        left_val = self._get_integer_value(left)
        right_val = self._get_integer_value(right)

        # x + 0 = x, 0 + x = x
        if op == BinaryOp.ADD:
            if right_val == 0:
                return left
            if left_val == 0:
                return right

        # x - 0 = x
        elif op == BinaryOp.SUB:
            if right_val == 0:
                return left
            # x - x = 0
            if self._exprs_equal(left, right):
                return IntegerLiteral(value=0, text="0")

        # x * 1 = x, 1 * x = x
        elif op == BinaryOp.MUL:
            if right_val == 1:
                return left
            if left_val == 1:
                return right
            # x * 0 = 0, 0 * x = 0
            if right_val == 0:
                return IntegerLiteral(value=0, text="0")
            if left_val == 0:
                return IntegerLiteral(value=0, text="0")

        # x / 1 = x
        elif op == BinaryOp.DIV:
            if right_val == 1:
                return left
            # x / x = 1 (if x != 0, but we can't prove that at compile time easily)

        # x mod 1 = 0
        elif op == BinaryOp.MOD:
            if right_val == 1:
                return IntegerLiteral(value=0, text="0")

        # x ** 0 = 1, x ** 1 = x
        # Note: x ** 0 = 1 only safe when x is a known integer (not Float64)
        # because IntegerLiteral(1) is wrong type for Float64 ** 0 which should be 1.0
        elif op == BinaryOp.EXP:
            if right_val == 0 and left_val is not None:
                # Only fold when left is also a constant integer
                return IntegerLiteral(value=1, text="1")
            if right_val == 1:
                # x ** 1 = x is type-safe for any x
                return left

        # Boolean: x and True = x, x and False = False
        elif op == BinaryOp.AND:
            if self._is_true(right):
                return left
            if self._is_true(left):
                return right
            if self._is_false(right) or self._is_false(left):
                return Identifier(name="False")

        # Boolean: x or False = x, x or True = True
        elif op == BinaryOp.OR:
            if self._is_false(right):
                return left
            if self._is_false(left):
                return right
            if self._is_true(right) or self._is_true(left):
                return Identifier(name="True")

        # Boolean: x xor False = x
        elif op == BinaryOp.XOR:
            if self._is_false(right):
                return left
            if self._is_false(left):
                return right

        return None

    # =========================================================================
    # Strength Reduction
    # =========================================================================

    def _strength_reduce(self, op: BinaryOp, left: Expr, right: Expr) -> Optional[Expr]:
        """Apply strength reduction (convert expensive ops to cheaper ones)."""
        right_val = self._get_integer_value(right)

        if right_val is not None and right_val > 0:
            # Check if right is a power of 2
            if self._is_power_of_2(right_val):
                shift = self._log2(right_val)

                # x * 2^n = x << n (left shift)
                if op == BinaryOp.MUL:
                    # We don't have shift in Ada syntax directly, but we can
                    # leave a hint for the code generator or use * 2 chains
                    # For now, skip this as Ada doesn't have shift operators
                    pass

                # x / 2^n = x >> n (right shift, for positive x)
                # x mod 2^n = x and (2^n - 1)
                # These would require runtime type info to ensure safety

        return None

    # =========================================================================
    # Boolean Simplifications
    # =========================================================================

    def _boolean_simplify(self, op: BinaryOp, left: Expr, right: Expr) -> Optional[Expr]:
        """Apply boolean-specific simplifications."""
        # x = x -> True
        if op == BinaryOp.EQ and self._exprs_equal(left, right):
            return Identifier(name="True")

        # x /= x -> False
        if op == BinaryOp.NE and self._exprs_equal(left, right):
            return Identifier(name="False")

        # x < x -> False, x > x -> False
        if op in (BinaryOp.LT, BinaryOp.GT) and self._exprs_equal(left, right):
            return Identifier(name="False")

        # x <= x -> True, x >= x -> True
        if op in (BinaryOp.LE, BinaryOp.GE) and self._exprs_equal(left, right):
            return Identifier(name="True")

        return None

    # =========================================================================
    # Ada-Specific: Attribute Evaluation
    # =========================================================================

    def _try_evaluate_attribute(
        self, prefix: Expr, attribute: str, args: list[Expr]
    ) -> Optional[Expr]:
        """Try to evaluate an attribute at compile time."""
        attr_upper = attribute.upper()

        # For range expressions used as prefix (type ranges)
        if isinstance(prefix, RangeExpr):
            low_val = self._get_integer_value(prefix.low)
            high_val = self._get_integer_value(prefix.high)

            if low_val is not None and high_val is not None:
                if attr_upper == "FIRST":
                    return IntegerLiteral(value=low_val, text=str(low_val))
                elif attr_upper == "LAST":
                    return IntegerLiteral(value=high_val, text=str(high_val))
                elif attr_upper == "LENGTH":
                    length = high_val - low_val + 1
                    return IntegerLiteral(value=length, text=str(length))

        # 'Size for integer literals (bits needed to represent value)
        if attr_upper == "SIZE" and isinstance(prefix, IntegerLiteral):
            val = prefix.value
            if val == 0:
                return IntegerLiteral(value=1, text="1")  # At least 1 bit
            size = val.bit_length()
            return IntegerLiteral(value=size, text=str(size))

        # 'Pos for character literals - returns code point
        if attr_upper == "POS" and args and isinstance(args[0], CharacterLiteral):
            char = args[0].value
            if len(char) == 1:
                pos = ord(char)
                self.stats.attribute_evaluations += 1
                return IntegerLiteral(value=pos, text=str(pos))

        # 'Val for integers with known prefix type (Character)
        if attr_upper == "VAL" and args and isinstance(args[0], IntegerLiteral):
            val = args[0].value
            # If prefix looks like Character type, convert to char literal
            if isinstance(prefix, Identifier) and prefix.name.upper() == "CHARACTER":
                if 0 <= val <= 127:  # ASCII range
                    char = chr(val)
                    self.stats.attribute_evaluations += 1
                    return CharacterLiteral(value=char)

        # 'Image for integer literals
        if attr_upper == "IMAGE" and isinstance(prefix, IntegerLiteral):
            text = str(prefix.value)
            self.stats.attribute_evaluations += 1
            return StringLiteral(value=text)

        # 'Image with argument for integer type
        if attr_upper == "IMAGE" and args and isinstance(args[0], IntegerLiteral):
            text = str(args[0].value)
            self.stats.attribute_evaluations += 1
            return StringLiteral(value=text)

        # 'Min and 'Max for constant pairs
        if attr_upper in ("MIN", "MAX") and len(args) == 2:
            left_val = self._get_integer_value(args[0])
            right_val = self._get_integer_value(args[1])
            if left_val is not None and right_val is not None:
                result = min(left_val, right_val) if attr_upper == "MIN" else max(left_val, right_val)
                self.stats.attribute_evaluations += 1
                return IntegerLiteral(value=result, text=str(result))

        # 'Succ and 'Pred for integer literals
        if attr_upper == "SUCC" and args and isinstance(args[0], IntegerLiteral):
            result = args[0].value + 1
            self.stats.attribute_evaluations += 1
            return IntegerLiteral(value=result, text=str(result))

        if attr_upper == "PRED" and args and isinstance(args[0], IntegerLiteral):
            result = args[0].value - 1
            self.stats.attribute_evaluations += 1
            return IntegerLiteral(value=result, text=str(result))

        # 'Abs for integer literals
        if attr_upper == "ABS" and args and isinstance(args[0], IntegerLiteral):
            result = abs(args[0].value)
            self.stats.attribute_evaluations += 1
            return IntegerLiteral(value=result, text=str(result))

        return None

    # =========================================================================
    # Helper Methods
    # =========================================================================

    def _unwrap_parens(self, expr: Expr) -> Expr:
        """Remove parentheses wrapper if present."""
        while isinstance(expr, Parenthesized):
            expr = expr.expr
        return expr

    def _get_integer_value(self, expr: Expr) -> Optional[int]:
        """Extract integer value from expression if constant."""
        if isinstance(expr, IntegerLiteral):
            return expr.value
        if isinstance(expr, Parenthesized):
            return self._get_integer_value(expr.expr)
        return None

    def _try_evaluate_constant(self, expr: Expr) -> Optional[Any]:
        """Try to evaluate an expression to a constant value."""
        if isinstance(expr, IntegerLiteral):
            return expr.value
        if isinstance(expr, Identifier):
            name = expr.name.upper()
            if name == "TRUE":
                return True
            if name == "FALSE":
                return False
        if isinstance(expr, BinaryExpr):
            left = self._try_evaluate_constant(expr.left)
            right = self._try_evaluate_constant(expr.right)
            if left is not None and right is not None:
                return self._eval_binary_int(expr.op, left, right)
        return None

    # Alias for _try_evaluate_constant
    _get_constant_value = _try_evaluate_constant

    def _try_evaluate_boolean(self, expr: Expr) -> Optional[bool]:
        """Try to evaluate a boolean expression at compile time."""
        if isinstance(expr, Identifier):
            name = expr.name.upper()
            if name == "TRUE":
                return True
            elif name == "FALSE":
                return False
        if isinstance(expr, Parenthesized):
            return self._try_evaluate_boolean(expr.expr)
        return None

    def _is_true(self, expr: Expr) -> bool:
        """Check if expression is the literal True."""
        return isinstance(expr, Identifier) and expr.name.upper() == "TRUE"

    def _is_false(self, expr: Expr) -> bool:
        """Check if expression is the literal False."""
        return isinstance(expr, Identifier) and expr.name.upper() == "FALSE"

    def _exprs_equal(self, a: Expr, b: Expr) -> bool:
        """Check if two expressions are structurally equal.

        This is used for CSE (Common Subexpression Elimination) and
        boolean simplifications like x = x -> True.
        """
        # Unwrap parentheses for comparison
        a = self._unwrap_parens(a)
        b = self._unwrap_parens(b)

        if type(a) != type(b):
            return False

        if isinstance(a, Identifier):
            return a.name.upper() == b.name.upper()

        if isinstance(a, IntegerLiteral):
            return a.value == b.value

        if isinstance(a, RealLiteral):
            return a.value == b.value

        if isinstance(a, CharacterLiteral):
            return a.value == b.value

        if isinstance(a, StringLiteral):
            return a.value == b.value

        if isinstance(a, NullLiteral):
            return True  # All null literals are equal

        if isinstance(a, BinaryExpr):
            return (a.op == b.op and
                    self._exprs_equal(a.left, b.left) and
                    self._exprs_equal(a.right, b.right))

        if isinstance(a, UnaryExpr):
            return (a.op == b.op and
                    self._exprs_equal(a.operand, b.operand))

        if isinstance(a, IndexedComponent):
            if not self._exprs_equal(a.prefix, b.prefix):
                return False
            if len(a.indices) != len(b.indices):
                return False
            return all(self._exprs_equal(ai, bi) for ai, bi in zip(a.indices, b.indices))

        if isinstance(a, AttributeReference):
            return (a.attribute.upper() == b.attribute.upper() and
                    self._exprs_equal(a.prefix, b.prefix) and
                    len(a.args) == len(b.args) and
                    all(self._exprs_equal(ai, bi) for ai, bi in zip(a.args, b.args)))

        if isinstance(a, FunctionCall):
            if not self._exprs_equal(a.name, b.name):
                return False
            if len(a.args) != len(b.args):
                return False
            for arg_a, arg_b in zip(a.args, b.args):
                if hasattr(arg_a, 'value') and hasattr(arg_b, 'value'):
                    if not self._exprs_equal(arg_a.value, arg_b.value):
                        return False
                elif arg_a != arg_b:
                    return False
            return True

        if isinstance(a, TypeConversion):
            return (self._exprs_equal(a.type_mark, b.type_mark) and
                    self._exprs_equal(a.expr, b.expr))

        if isinstance(a, QualifiedExpr):
            return (self._exprs_equal(a.type_mark, b.type_mark) and
                    self._exprs_equal(a.expr, b.expr))

        if isinstance(a, RangeExpr):
            return (self._exprs_equal(a.low, b.low) and
                    self._exprs_equal(a.high, b.high))

        # For complex expressions we don't yet handle, be conservative
        return False

    def _is_power_of_2(self, n: int) -> bool:
        """Check if n is a power of 2."""
        return n > 0 and (n & (n - 1)) == 0

    def _log2(self, n: int) -> int:
        """Return log base 2 of n (assumes n is power of 2)."""
        result = 0
        while n > 1:
            n >>= 1
            result += 1
        return result


def optimize(program: Program, level: int = 2) -> tuple[Program, OptimizationStats]:
    """
    Convenience function to optimize a program.

    Args:
        program: The AST to optimize
        level: Optimization level (0-3)

    Returns:
        Tuple of (optimized program, statistics)
    """
    config = OptimizerConfig.for_level(level)
    optimizer = ASTOptimizer(config)
    optimized = optimizer.optimize(program)
    return optimized, optimizer.stats
