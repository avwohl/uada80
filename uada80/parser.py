"""
Recursive descent parser for Ada.

Implements a complete parser for Ada 2012 based on the Ada Reference Manual grammar.
"""

from typing import Optional
from .lexer import Token, TokenType, Lexer
from .ast_nodes import *


class ParseError(Exception):
    """Parse error exception."""

    def __init__(self, message: str, token: Optional[Token] = None) -> None:
        self.message = message
        self.token = token
        if token:
            super().__init__(f"{token.location}: {message}")
        else:
            super().__init__(message)


class Parser:
    """
    Recursive descent parser for Ada.

    Parses tokens into an Abstract Syntax Tree (AST).
    """

    def __init__(self, tokens: list[Token]) -> None:
        self.tokens = tokens
        self.pos = 0
        self.current = tokens[0] if tokens else Token(TokenType.EOF, "", None)

    def peek(self, offset: int = 0) -> Token:
        """Peek at token at current position + offset."""
        pos = self.pos + offset
        if pos < len(self.tokens):
            return self.tokens[pos]
        return self.tokens[-1]  # EOF

    def advance(self) -> Token:
        """Consume and return current token."""
        token = self.current
        if self.pos < len(self.tokens) - 1:
            self.pos += 1
            self.current = self.tokens[self.pos]
        return token

    def check(self, *types: TokenType) -> bool:
        """Check if current token is one of the given types."""
        return self.current.type in types

    def match(self, *types: TokenType) -> bool:
        """If current token matches, consume it and return True."""
        if self.check(*types):
            self.advance()
            return True
        return False

    def expect(self, token_type: TokenType, message: Optional[str] = None) -> Token:
        """Expect a specific token type, raise error if not found."""
        if not self.check(token_type):
            if message:
                raise ParseError(message, self.current)
            raise ParseError(f"Expected {token_type.name}, got {self.current.type.name}", self.current)
        return self.advance()

    def expect_identifier(self, message: str = "Expected identifier") -> str:
        """Expect an identifier and return its name."""
        token = self.expect(TokenType.IDENTIFIER, message)
        return token.value

    def synchronize(self) -> None:
        """Synchronize parser after error (skip to next statement/declaration)."""
        while not self.check(TokenType.EOF):
            if self.check(
                TokenType.PROCEDURE,
                TokenType.FUNCTION,
                TokenType.PACKAGE,
                TokenType.TYPE,
                TokenType.SUBTYPE,
                TokenType.BEGIN,
                TokenType.END,
            ):
                return
            self.advance()

    def make_span(self, start_token: Token, end_token: Optional[Token] = None) -> SourceSpan:
        """Create a source span from start to end token."""
        if end_token is None:
            end_token = self.current
        return SourceSpan(
            filename=start_token.location.filename,
            start_line=start_token.location.line,
            start_column=start_token.location.column,
            end_line=end_token.location.line,
            end_column=end_token.location.column,
        )

    # ========================================================================
    # Top-Level Parsing
    # ========================================================================

    def parse(self) -> Program:
        """Parse a complete Ada program (one or more compilation units)."""
        units = []
        while not self.check(TokenType.EOF):
            try:
                unit = self.parse_compilation_unit()
                units.append(unit)
            except ParseError as e:
                print(f"Parse error: {e}")
                self.synchronize()

        return Program(units=units)

    def parse_compilation_unit(self) -> CompilationUnit:
        """Parse a compilation unit."""
        start = self.current

        # Context clauses (with/use)
        context = self.parse_context_clause()

        # Main unit (package, subprogram, generic, etc.)
        unit = self.parse_library_item()

        return CompilationUnit(
            context_clauses=context, unit=unit, span=self.make_span(start)
        )

    def parse_context_clause(self) -> list[WithClause | UseClause]:
        """Parse context clause (with and use clauses)."""
        clauses = []

        while self.check(TokenType.WITH, TokenType.USE):
            if self.match(TokenType.WITH):
                clauses.append(self.parse_with_clause())
            elif self.match(TokenType.USE):
                clauses.append(self.parse_use_clause())

        return clauses

    def parse_with_clause(self) -> WithClause:
        """Parse with clause."""
        start = self.current
        names = [self.parse_name()]

        while self.match(TokenType.COMMA):
            names.append(self.parse_name())

        self.expect(TokenType.SEMICOLON)

        return WithClause(names=names, span=self.make_span(start))

    def parse_use_clause(self) -> UseClause:
        """Parse use clause."""
        start = self.current
        is_type = self.match(TokenType.TYPE)

        names = [self.parse_name()]
        while self.match(TokenType.COMMA):
            names.append(self.parse_name())

        self.expect(TokenType.SEMICOLON)

        return UseClause(names=names, is_type=is_type, span=self.make_span(start))

    def parse_library_item(self) -> Decl:
        """Parse a library item (package, subprogram, generic, etc.)."""
        # Check for generic
        if self.check(TokenType.GENERIC):
            return self.parse_generic_declaration()

        # Package or subprogram
        if self.check(TokenType.PACKAGE):
            return self.parse_package()
        elif self.check(TokenType.PROCEDURE, TokenType.FUNCTION):
            return self.parse_subprogram()
        else:
            raise ParseError("Expected package, subprogram, or generic", self.current)

    # ========================================================================
    # Names and Expressions
    # ========================================================================

    def parse_qualified_name(self) -> Expr:
        """
        Parse a qualified name (identifier or selected component only).

        This is used for generic names where we don't want to consume
        function call syntax (parentheses) as part of the name.
        """
        start = self.current
        name: Expr = Identifier(name=self.expect_identifier(), span=self.make_span(start))

        # Handle only dot-selection, not parentheses or attributes
        while self.match(TokenType.DOT):
            component = self.expect_identifier()
            name = SelectedComponent(
                prefix=name, selector=component, span=self.make_span(start)
            )

        return name

    def parse_name(self) -> Expr:
        """
        Parse a name (identifier, selected name, indexed component, etc.).

        Ada grammar for names is quite complex, handling:
        - Simple identifiers
        - Selected components (Package.Entity)
        - Indexed components (Array(I))
        - Slices (Array(1..10))
        - Attribute references (X'First)
        - Function calls (which look like indexed components)
        """
        start = self.current
        name: Expr = Identifier(name=self.expect_identifier(), span=self.make_span(start))

        # Handle suffixes (dot, apostrophe, parentheses)
        while True:
            if self.match(TokenType.DOT):
                # Selected component
                selector = self.expect_identifier()
                name = SelectedName(prefix=name, selector=selector, span=self.make_span(start))

            elif self.match(TokenType.APOSTROPHE):
                # Attribute reference
                attr_name = self.expect_identifier()
                args = []
                if self.match(TokenType.LEFT_PAREN):
                    args = self.parse_expression_list()
                    self.expect(TokenType.RIGHT_PAREN)
                name = AttributeReference(
                    prefix=name, attribute=attr_name, args=args, span=self.make_span(start)
                )

            elif self.match(TokenType.LEFT_PAREN):
                # Either indexed component, slice, or function call
                # Parse first argument/index
                first_expr = self.parse_expression()

                if self.match(TokenType.DOUBLE_DOT):
                    # It's a slice
                    high = self.parse_expression()
                    self.expect(TokenType.RIGHT_PAREN)
                    range_expr = RangeExpr(low=first_expr, high=high, span=self.make_span(start))
                    name = Slice(prefix=name, range_expr=range_expr, span=self.make_span(start))
                else:
                    # Indexed component or function call (we'll treat as indexed for now)
                    # Parser can't always distinguish - semantic analysis will resolve
                    indices = [first_expr]
                    while self.match(TokenType.COMMA):
                        indices.append(self.parse_expression())
                    self.expect(TokenType.RIGHT_PAREN)
                    name = IndexedComponent(prefix=name, indices=indices, span=self.make_span(start))

            else:
                # No more suffixes
                break

        return name

    def parse_expression(self) -> Expr:
        """Parse an expression (entry point for expression parsing)."""
        return self.parse_logical_or()

    def parse_logical_or(self) -> Expr:
        """Parse logical OR expression."""
        start = self.current
        left = self.parse_logical_xor()

        while self.check(TokenType.OR):
            # Check for 'or else' (short-circuit)
            if self.peek(1).type == TokenType.ELSE:
                self.advance()  # or
                self.advance()  # else
                right = self.parse_logical_xor()
                left = BinaryExpr(
                    op=BinaryOp.OR_ELSE, left=left, right=right, span=self.make_span(start)
                )
            else:
                self.advance()  # or
                right = self.parse_logical_xor()
                left = BinaryExpr(op=BinaryOp.OR, left=left, right=right, span=self.make_span(start))

        return left

    def parse_logical_xor(self) -> Expr:
        """Parse logical XOR expression."""
        start = self.current
        left = self.parse_logical_and()

        while self.match(TokenType.XOR):
            right = self.parse_logical_and()
            left = BinaryExpr(op=BinaryOp.XOR, left=left, right=right, span=self.make_span(start))

        return left

    def parse_logical_and(self) -> Expr:
        """Parse logical AND expression."""
        start = self.current
        left = self.parse_relational()

        while self.check(TokenType.AND):
            # Check for 'and then' (short-circuit)
            if self.peek(1).type == TokenType.THEN:
                self.advance()  # and
                self.advance()  # then
                right = self.parse_relational()
                left = BinaryExpr(
                    op=BinaryOp.AND_THEN, left=left, right=right, span=self.make_span(start)
                )
            else:
                self.advance()  # and
                right = self.parse_relational()
                left = BinaryExpr(op=BinaryOp.AND, left=left, right=right, span=self.make_span(start))

        return left

    def parse_relational(self) -> Expr:
        """Parse relational expression."""
        start = self.current
        left = self.parse_additive()

        # Relational operators
        if self.match(TokenType.EQUAL):
            right = self.parse_additive()
            return BinaryExpr(op=BinaryOp.EQ, left=left, right=right, span=self.make_span(start))
        elif self.match(TokenType.NOT_EQUAL):
            right = self.parse_additive()
            return BinaryExpr(op=BinaryOp.NE, left=left, right=right, span=self.make_span(start))
        elif self.match(TokenType.LESS):
            right = self.parse_additive()
            return BinaryExpr(op=BinaryOp.LT, left=left, right=right, span=self.make_span(start))
        elif self.match(TokenType.LESS_EQUAL):
            right = self.parse_additive()
            return BinaryExpr(op=BinaryOp.LE, left=left, right=right, span=self.make_span(start))
        elif self.match(TokenType.GREATER):
            right = self.parse_additive()
            return BinaryExpr(op=BinaryOp.GT, left=left, right=right, span=self.make_span(start))
        elif self.match(TokenType.GREATER_EQUAL):
            right = self.parse_additive()
            return BinaryExpr(op=BinaryOp.GE, left=left, right=right, span=self.make_span(start))
        elif self.match(TokenType.IN):
            # Membership test
            is_not = False
            choices = []
            # Parse range or list
            if self.check(TokenType.INTEGER_LITERAL, TokenType.IDENTIFIER):
                first_expr = self.parse_additive()
                if self.match(TokenType.DOUBLE_DOT):
                    high = self.parse_additive()
                    range_expr = RangeExpr(low=first_expr, high=high)
                    choices.append(RangeChoice(range_expr=range_expr))
                else:
                    choices.append(ExprChoice(expr=first_expr))
            return MembershipTest(expr=left, is_not=is_not, choices=choices, span=self.make_span(start))
        elif self.check(TokenType.NOT) and self.peek(1).type == TokenType.IN:
            self.advance()  # not
            self.advance()  # in
            # Similar to IN
            choices = []
            first_expr = self.parse_additive()
            if self.match(TokenType.DOUBLE_DOT):
                high = self.parse_additive()
                range_expr = RangeExpr(low=first_expr, high=high)
                choices.append(RangeChoice(range_expr=range_expr))
            else:
                choices.append(ExprChoice(expr=first_expr))
            return MembershipTest(expr=left, is_not=True, choices=choices, span=self.make_span(start))

        return left

    def parse_additive(self) -> Expr:
        """Parse additive expression (+, -, &)."""
        start = self.current
        left = self.parse_multiplicative()

        while True:
            if self.match(TokenType.PLUS):
                right = self.parse_multiplicative()
                left = BinaryExpr(op=BinaryOp.ADD, left=left, right=right, span=self.make_span(start))
            elif self.match(TokenType.MINUS):
                right = self.parse_multiplicative()
                left = BinaryExpr(op=BinaryOp.SUB, left=left, right=right, span=self.make_span(start))
            elif self.match(TokenType.AMPERSAND):
                right = self.parse_multiplicative()
                left = BinaryExpr(op=BinaryOp.CONCAT, left=left, right=right, span=self.make_span(start))
            else:
                break

        return left

    def parse_multiplicative(self) -> Expr:
        """Parse multiplicative expression (*, /, mod, rem)."""
        start = self.current
        left = self.parse_exponential()

        while True:
            if self.match(TokenType.STAR):
                right = self.parse_exponential()
                left = BinaryExpr(op=BinaryOp.MUL, left=left, right=right, span=self.make_span(start))
            elif self.match(TokenType.SLASH):
                right = self.parse_exponential()
                left = BinaryExpr(op=BinaryOp.DIV, left=left, right=right, span=self.make_span(start))
            elif self.match(TokenType.MOD):
                right = self.parse_exponential()
                left = BinaryExpr(op=BinaryOp.MOD, left=left, right=right, span=self.make_span(start))
            elif self.match(TokenType.REM):
                right = self.parse_exponential()
                left = BinaryExpr(op=BinaryOp.REM, left=left, right=right, span=self.make_span(start))
            else:
                break

        return left

    def parse_exponential(self) -> Expr:
        """Parse exponential expression (**)."""
        start = self.current
        left = self.parse_unary()

        if self.match(TokenType.DOUBLE_STAR):
            right = self.parse_exponential()  # Right associative
            return BinaryExpr(op=BinaryOp.EXP, left=left, right=right, span=self.make_span(start))

        return left

    def parse_unary(self) -> Expr:
        """Parse unary expression (+, -, not, abs)."""
        start = self.current

        if self.match(TokenType.PLUS):
            operand = self.parse_unary()
            return UnaryExpr(op=UnaryOp.PLUS, operand=operand, span=self.make_span(start))
        elif self.match(TokenType.MINUS):
            operand = self.parse_unary()
            return UnaryExpr(op=UnaryOp.MINUS, operand=operand, span=self.make_span(start))
        elif self.match(TokenType.NOT):
            operand = self.parse_unary()
            return UnaryExpr(op=UnaryOp.NOT, operand=operand, span=self.make_span(start))
        elif self.match(TokenType.ABS):
            operand = self.parse_unary()
            return UnaryExpr(op=UnaryOp.ABS, operand=operand, span=self.make_span(start))

        return self.parse_primary()

    def parse_primary(self) -> Expr:
        """Parse primary expression (literals, names, aggregates, etc.)."""
        start = self.current

        # Literals
        if self.check(TokenType.INTEGER_LITERAL):
            text = self.current.value
            value = self.parse_integer_literal(text)
            self.advance()
            return IntegerLiteral(value=value, text=text, span=self.make_span(start))

        if self.check(TokenType.REAL_LITERAL):
            text = self.current.value
            value = float(text.replace("_", ""))
            self.advance()
            return RealLiteral(value=value, text=text, span=self.make_span(start))

        if self.check(TokenType.STRING_LITERAL):
            value = self.current.value
            self.advance()
            return StringLiteral(value=value, span=self.make_span(start))

        if self.check(TokenType.CHARACTER_LITERAL):
            value = self.current.value
            self.advance()
            return CharacterLiteral(value=value, span=self.make_span(start))

        if self.match(TokenType.NULL):
            return NullLiteral(span=self.make_span(start))

        # Parenthesized expression or aggregate
        if self.match(TokenType.LEFT_PAREN):
            # Could be aggregate or parenthesized expression
            # Check for 'others' which definitely means aggregate
            if self.check(TokenType.OTHERS):
                components = self.parse_aggregate_components()
                self.expect(TokenType.RIGHT_PAREN)
                return Aggregate(components=components, span=self.make_span(start))

            # Parse first expression
            first_expr = self.parse_expression()

            # Check what follows to determine aggregate vs parenthesized
            if self.match(TokenType.ARROW):
                # Named aggregate: (field => value, ...)
                choices = [ExprChoice(expr=first_expr)]
                value = self.parse_expression()
                components = [ComponentAssociation(choices=choices, value=value)]

                while self.match(TokenType.COMMA):
                    comp = self.parse_aggregate_component()
                    components.append(comp)

                self.expect(TokenType.RIGHT_PAREN)
                return Aggregate(components=components, span=self.make_span(start))

            elif self.match(TokenType.COMMA):
                # Positional aggregate: (val1, val2, ...)
                components = [ComponentAssociation(choices=[], value=first_expr)]

                while True:
                    expr = self.parse_expression()
                    components.append(ComponentAssociation(choices=[], value=expr))
                    if not self.match(TokenType.COMMA):
                        break

                self.expect(TokenType.RIGHT_PAREN)
                return Aggregate(components=components, span=self.make_span(start))

            else:
                # Simple parenthesized expression
                self.expect(TokenType.RIGHT_PAREN)
                return Parenthesized(expr=first_expr, span=self.make_span(start))

        # Allocator (new Type)
        if self.match(TokenType.NEW):
            type_mark = self.parse_name()
            init_value = None
            if self.match(TokenType.APOSTROPHE):
                self.expect(TokenType.LEFT_PAREN)
                init_value = self.parse_expression()
                self.expect(TokenType.RIGHT_PAREN)
            return Allocator(type_mark=type_mark, initial_value=init_value, span=self.make_span(start))

        # Names (identifiers, function calls, etc.)
        if self.check(TokenType.IDENTIFIER):
            return self.parse_name()

        raise ParseError(f"Unexpected token in expression: {self.current.type.name}", self.current)

    def parse_integer_literal(self, text: str) -> int:
        """Parse Ada integer literal (handles based literals)."""
        text = text.replace("_", "")

        # Based literal: base#value#
        if "#" in text:
            parts = text.split("#")
            base = int(parts[0])
            value_str = parts[1]
            return int(value_str, base)

        # Decimal
        return int(text)

    def parse_expression_list(self) -> list[Expr]:
        """Parse comma-separated list of expressions."""
        exprs = [self.parse_expression()]
        while self.match(TokenType.COMMA):
            exprs.append(self.parse_expression())
        return exprs

    def parse_aggregate_components(self) -> list[ComponentAssociation]:
        """Parse aggregate component associations."""
        components = []

        if self.match(TokenType.OTHERS):
            self.expect(TokenType.ARROW)
            value = self.parse_expression()
            components.append(
                ComponentAssociation(choices=[OthersChoice()], value=value)
            )
        else:
            while True:
                comp = self.parse_aggregate_component()
                components.append(comp)

                if not self.match(TokenType.COMMA):
                    break

        return components

    def parse_aggregate_component(self) -> ComponentAssociation:
        """Parse a single aggregate component association."""
        # Check for 'others'
        if self.match(TokenType.OTHERS):
            self.expect(TokenType.ARROW)
            value = self.parse_expression()
            return ComponentAssociation(choices=[OthersChoice()], value=value)

        # Parse choice(s) => expression or positional
        choices: list[Choice] = []
        first_expr = self.parse_expression()

        if self.match(TokenType.ARROW):
            # Named association
            choices.append(ExprChoice(expr=first_expr))
            while self.match(TokenType.PIPE):
                choices.append(ExprChoice(expr=self.parse_expression()))
            value = self.parse_expression()
            return ComponentAssociation(choices=choices, value=value)
        else:
            # Positional association
            return ComponentAssociation(choices=[], value=first_expr)

    # ========================================================================
    # Statements
    # ========================================================================

    def parse_statement(self) -> Stmt:
        """Parse a single statement."""
        start = self.current

        # Null statement
        if self.match(TokenType.NULL):
            self.expect(TokenType.SEMICOLON)
            return NullStmt(span=self.make_span(start))

        # If statement
        if self.check(TokenType.IF):
            return self.parse_if_statement()

        # Case statement
        if self.check(TokenType.CASE):
            return self.parse_case_statement()

        # Loop statement
        if self.check(TokenType.LOOP, TokenType.WHILE, TokenType.FOR):
            return self.parse_loop_statement()

        # Block statement
        if self.check(TokenType.DECLARE):
            return self.parse_block_statement()

        # Exit statement
        if self.match(TokenType.EXIT):
            return self.parse_exit_statement()

        # Return statement
        if self.match(TokenType.RETURN):
            return self.parse_return_statement()

        # Goto statement
        if self.match(TokenType.GOTO):
            label = self.expect_identifier()
            self.expect(TokenType.SEMICOLON)
            return GotoStmt(label=label, span=self.make_span(start))

        # Raise statement
        if self.match(TokenType.RAISE):
            return self.parse_raise_statement()

        # Delay statement
        if self.match(TokenType.DELAY):
            return self.parse_delay_statement()

        # Accept statement
        if self.match(TokenType.ACCEPT):
            return self.parse_accept_statement()

        # Select statement
        if self.match(TokenType.SELECT):
            return self.parse_select_statement()

        # Abort statement
        if self.match(TokenType.ABORT):
            task_names = [self.parse_name()]
            while self.match(TokenType.COMMA):
                task_names.append(self.parse_name())
            self.expect(TokenType.SEMICOLON)
            return AbortStmt(task_names=task_names, span=self.make_span(start))

        # Requeue statement
        if self.match(TokenType.REQUEUE):
            entry_name = self.parse_name()
            is_with_abort = False
            if self.match(TokenType.WITH):
                self.expect(TokenType.ABORT)
                is_with_abort = True
            self.expect(TokenType.SEMICOLON)
            return RequeueStmt(entry_name=entry_name, is_with_abort=is_with_abort, span=self.make_span(start))

        # Pragma
        if self.match(TokenType.PRAGMA):
            name = self.expect_identifier()
            args = []
            if self.match(TokenType.LEFT_PAREN):
                args = self.parse_expression_list()
                self.expect(TokenType.RIGHT_PAREN)
            self.expect(TokenType.SEMICOLON)
            return PragmaStmt(name=name, args=args, span=self.make_span(start))

        # Assignment or procedure call (disambiguate)
        # Both start with a name, so parse name first
        name = self.parse_name()

        if self.match(TokenType.ASSIGN):
            # Assignment statement
            value = self.parse_expression()
            self.expect(TokenType.SEMICOLON)
            return AssignmentStmt(target=name, value=value, span=self.make_span(start))
        else:
            # Procedure call (name is actually the call)
            # Check if it's a function call node, convert to procedure call
            if isinstance(name, IndexedComponent):
                # Convert indexed component to procedure call
                args = [ActualParameter(value=idx) for idx in name.indices]
                self.expect(TokenType.SEMICOLON)
                return ProcedureCallStmt(name=name.prefix, args=args, span=self.make_span(start))
            else:
                # Simple procedure call without parameters
                self.expect(TokenType.SEMICOLON)
                return ProcedureCallStmt(name=name, args=[], span=self.make_span(start))

    def parse_if_statement(self) -> IfStmt:
        """Parse if statement."""
        start = self.current
        self.expect(TokenType.IF)

        condition = self.parse_expression()
        self.expect(TokenType.THEN)
        then_stmts = self.parse_statement_sequence()

        elsif_parts = []
        while self.match(TokenType.ELSIF):
            elsif_cond = self.parse_expression()
            self.expect(TokenType.THEN)
            elsif_stmts = self.parse_statement_sequence()
            elsif_parts.append((elsif_cond, elsif_stmts))

        else_stmts = []
        if self.match(TokenType.ELSE):
            else_stmts = self.parse_statement_sequence()

        self.expect(TokenType.END)
        self.expect(TokenType.IF)
        self.expect(TokenType.SEMICOLON)

        return IfStmt(
            condition=condition,
            then_stmts=then_stmts,
            elsif_parts=elsif_parts,
            else_stmts=else_stmts,
            span=self.make_span(start),
        )

    def parse_case_statement(self) -> CaseStmt:
        """Parse case statement."""
        start = self.current
        self.expect(TokenType.CASE)

        expr = self.parse_expression()
        self.expect(TokenType.IS)

        alternatives = []
        while self.match(TokenType.WHEN):
            choices = self.parse_choice_list()
            self.expect(TokenType.ARROW)
            stmts = self.parse_statement_sequence()
            alternatives.append(CaseAlternative(choices=choices, statements=stmts))

        self.expect(TokenType.END)
        self.expect(TokenType.CASE)
        self.expect(TokenType.SEMICOLON)

        return CaseStmt(expr=expr, alternatives=alternatives, span=self.make_span(start))

    def parse_choice_list(self) -> list[Choice]:
        """Parse choice list (for case or aggregate)."""
        choices = []

        while True:
            if self.match(TokenType.OTHERS):
                choices.append(OthersChoice())
            else:
                expr = self.parse_expression()
                if self.match(TokenType.DOUBLE_DOT):
                    high = self.parse_expression()
                    choices.append(RangeChoice(range_expr=RangeExpr(low=expr, high=high)))
                else:
                    choices.append(ExprChoice(expr=expr))

            if not self.match(TokenType.PIPE):
                break

        return choices

    def parse_loop_statement(self) -> LoopStmt:
        """Parse loop statement."""
        start = self.current
        label = None

        # Optional label (handled in parse_statement_sequence)
        iteration_scheme = None

        # While loop
        if self.match(TokenType.WHILE):
            condition = self.parse_expression()
            iteration_scheme = WhileScheme(condition=condition)
            self.expect(TokenType.LOOP)

        # For loop
        elif self.match(TokenType.FOR):
            iterator = self.parse_iterator_spec()
            iteration_scheme = ForScheme(iterator=iterator)
            self.expect(TokenType.LOOP)

        # Plain loop
        else:
            self.expect(TokenType.LOOP)

        statements = self.parse_statement_sequence()

        self.expect(TokenType.END)
        self.expect(TokenType.LOOP)

        # Optional loop name
        if self.check(TokenType.IDENTIFIER):
            label = self.expect_identifier()

        self.expect(TokenType.SEMICOLON)

        return LoopStmt(
            iteration_scheme=iteration_scheme,
            statements=statements,
            label=label,
            span=self.make_span(start),
        )

    def parse_iterator_spec(self) -> IteratorSpec:
        """Parse iterator specification."""
        name = self.expect_identifier()
        self.expect(TokenType.IN)
        is_reverse = self.match(TokenType.REVERSE)
        iterable = self.parse_discrete_range_or_subtype()  # Can be range or iterable type

        return IteratorSpec(name=name, is_reverse=is_reverse, iterable=iterable)

    def parse_discrete_range_or_subtype(self) -> Expr:
        """Parse a discrete range (1..10) or a subtype indication (Integer)."""
        start = self.current
        first_expr = self.parse_additive()

        if self.match(TokenType.DOUBLE_DOT):
            # It's a discrete range
            high = self.parse_additive()
            return RangeExpr(low=first_expr, high=high, span=self.make_span(start))

        # It's a subtype indication or type mark
        return first_expr

    def parse_block_statement(self) -> BlockStmt:
        """Parse block statement."""
        start = self.current
        self.expect(TokenType.DECLARE)

        declarations = self.parse_declarative_part()

        self.expect(TokenType.BEGIN)
        statements = self.parse_statement_sequence()

        handlers = []
        if self.match(TokenType.EXCEPTION):
            handlers = self.parse_exception_handlers()

        self.expect(TokenType.END)
        self.expect(TokenType.SEMICOLON)

        return BlockStmt(
            declarations=declarations, statements=statements, handled_exception_handlers=handlers, span=self.make_span(start)
        )

    def parse_exit_statement(self) -> ExitStmt:
        """Parse exit statement."""
        start = self.current
        loop_label = None
        condition = None

        if self.check(TokenType.IDENTIFIER):
            loop_label = self.expect_identifier()

        if self.match(TokenType.WHEN):
            condition = self.parse_expression()

        self.expect(TokenType.SEMICOLON)

        return ExitStmt(loop_label=loop_label, condition=condition, span=self.make_span(start))

    def parse_return_statement(self) -> ReturnStmt:
        """Parse return statement."""
        start = self.current
        value = None

        if not self.check(TokenType.SEMICOLON):
            value = self.parse_expression()

        self.expect(TokenType.SEMICOLON)

        return ReturnStmt(value=value, span=self.make_span(start))

    def parse_raise_statement(self) -> RaiseStmt:
        """Parse raise statement."""
        start = self.current
        exception_name = None
        message = None

        if not self.check(TokenType.SEMICOLON):
            exception_name = self.parse_name()

            if self.match(TokenType.WITH):
                message = self.parse_expression()

        self.expect(TokenType.SEMICOLON)

        return RaiseStmt(exception_name=exception_name, message=message, span=self.make_span(start))

    def parse_delay_statement(self) -> DelayStmt:
        """Parse delay statement."""
        start = self.current
        is_until = self.match(TokenType.UNTIL)
        expression = self.parse_expression()
        self.expect(TokenType.SEMICOLON)

        return DelayStmt(is_until=is_until, expression=expression, span=self.make_span(start))

    def parse_accept_statement(self) -> AcceptStmt:
        """Parse accept statement."""
        start = self.current
        entry_name = self.expect_identifier()

        parameters = []
        if self.match(TokenType.LEFT_PAREN):
            parameters = self.parse_parameter_specifications()
            self.expect(TokenType.RIGHT_PAREN)

        statements = []
        if self.match(TokenType.DO):
            statements = self.parse_statement_sequence()
            self.expect(TokenType.END)
            if self.check(TokenType.IDENTIFIER):
                self.advance()  # Optional entry name

        self.expect(TokenType.SEMICOLON)

        return AcceptStmt(entry_name=entry_name, parameters=parameters, statements=statements, span=self.make_span(start))

    def parse_select_statement(self) -> SelectStmt:
        """Parse select statement."""
        start = self.current
        alternatives = []

        # Parse select alternatives
        while self.match(TokenType.WHEN):
            guard = self.parse_expression()
            self.expect(TokenType.ARROW)
            stmts = self.parse_statement_sequence()
            alternatives.append(SelectAlternative(guard=guard, statements=stmts))

        if self.match(TokenType.OR):
            while not self.check(TokenType.END):
                if self.match(TokenType.WHEN):
                    guard = self.parse_expression()
                    self.expect(TokenType.ARROW)
                    stmts = self.parse_statement_sequence()
                    alternatives.append(SelectAlternative(guard=guard, statements=stmts))
                else:
                    stmts = self.parse_statement_sequence()
                    alternatives.append(SelectAlternative(guard=None, statements=stmts))
                    break

        self.expect(TokenType.END)
        self.expect(TokenType.SELECT)
        self.expect(TokenType.SEMICOLON)

        return SelectStmt(alternatives=alternatives, span=self.make_span(start))

    def parse_statement_sequence(self) -> list[Stmt]:
        """Parse a sequence of statements."""
        statements = []

        while not self.check(
            TokenType.END,
            TokenType.ELSE,
            TokenType.ELSIF,
            TokenType.WHEN,
            TokenType.EXCEPTION,
            TokenType.OR,
            TokenType.EOF,
        ):
            stmt = self.parse_statement()
            statements.append(stmt)

        return statements

    def parse_exception_handlers(self) -> list[ExceptionHandler]:
        """Parse exception handlers."""
        handlers = []

        while self.match(TokenType.WHEN):
            exception_names = []

            if self.match(TokenType.OTHERS):
                exception_names = []
            else:
                exception_names.append(self.parse_name())
                while self.match(TokenType.PIPE):
                    exception_names.append(self.parse_name())

            self.expect(TokenType.ARROW)
            stmts = self.parse_statement_sequence()

            handlers.append(ExceptionHandler(exception_names=exception_names, statements=stmts))

        return handlers

    # ========================================================================
    # Declarations
    # ========================================================================

    def parse_declarative_part(self) -> list[Decl]:
        """Parse declarative part (sequence of declarations)."""
        declarations = []

        while not self.check(TokenType.BEGIN, TokenType.END, TokenType.PRIVATE, TokenType.EOF):
            try:
                decl = self.parse_declaration()
                if decl:
                    declarations.append(decl)
            except ParseError as e:
                print(f"Parse error in declaration: {e}")
                self.synchronize()

        return declarations

    def parse_declaration(self) -> Optional[Decl]:
        """Parse a single declaration."""
        start = self.current

        # Type declaration
        if self.check(TokenType.TYPE):
            return self.parse_type_declaration()

        # Subtype declaration
        if self.match(TokenType.SUBTYPE):
            return self.parse_subtype_declaration()

        # Object declaration (variables, constants)
        if self.check(TokenType.IDENTIFIER):
            return self.parse_object_declaration()

        # Subprogram declaration or body
        if self.check(TokenType.PROCEDURE, TokenType.FUNCTION):
            return self.parse_subprogram()

        # Package declaration or body
        if self.check(TokenType.PACKAGE):
            return self.parse_package()

        # Generic declaration
        if self.check(TokenType.GENERIC):
            return self.parse_generic_declaration()

        # Task declaration
        if self.check(TokenType.TASK):
            return self.parse_task_declaration()

        # Protected declaration
        if self.check(TokenType.PROTECTED):
            return self.parse_protected_declaration()

        # Exception declaration (looks like object declaration)
        # Handled in object declaration

        # Use clause
        if self.match(TokenType.USE):
            return self.parse_use_clause()

        # Pragma
        if self.match(TokenType.PRAGMA):
            name = self.expect_identifier()
            args = []
            if self.match(TokenType.LEFT_PAREN):
                args = self.parse_expression_list()
                self.expect(TokenType.RIGHT_PAREN)
            self.expect(TokenType.SEMICOLON)
            return PragmaStmt(name=name, args=args, span=self.make_span(start))

        return None

    def parse_type_declaration(self) -> TypeDecl:
        """Parse type declaration."""
        start = self.current
        self.expect(TokenType.TYPE)

        name = self.expect_identifier()

        # Discriminants
        discriminants = []
        if self.match(TokenType.LEFT_PAREN):
            discriminants = self.parse_discriminant_specifications()
            self.expect(TokenType.RIGHT_PAREN)

        is_abstract = False
        is_tagged = False
        is_limited = False

        # Type modifiers
        if self.match(TokenType.IS):
            if self.match(TokenType.ABSTRACT):
                is_abstract = True
            if self.match(TokenType.TAGGED):
                is_tagged = True
            if self.match(TokenType.LIMITED):
                is_limited = True

            # Parse type definition
            type_def = self.parse_type_definition()
        else:
            # Incomplete type declaration
            type_def = None

        self.expect(TokenType.SEMICOLON)

        return TypeDecl(
            name=name,
            type_def=type_def,
            discriminants=discriminants,
            is_abstract=is_abstract,
            is_tagged=is_tagged,
            is_limited=is_limited,
            span=self.make_span(start),
        )

    def parse_type_definition(self) -> TypeDef:
        """Parse type definition."""
        start = self.current

        # Range type (integer)
        if self.match(TokenType.RANGE):
            range_expr = None
            if self.check(TokenType.BOX):
                self.advance()  # <>
            else:
                low = self.parse_expression()
                self.expect(TokenType.DOUBLE_DOT)
                high = self.parse_expression()
                range_expr = RangeExpr(low=low, high=high)
            return IntegerTypeDef(range_constraint=range_expr)

        # Modular type (type X is mod N)
        if self.match(TokenType.MOD):
            modulus = self.parse_expression()
            return ModularTypeDef(modulus=modulus)

        # Enumeration type
        if self.match(TokenType.LEFT_PAREN):
            literals = []
            literals.append(self.expect_identifier())
            while self.match(TokenType.COMMA):
                literals.append(self.expect_identifier())
            self.expect(TokenType.RIGHT_PAREN)
            return EnumerationTypeDef(literals=literals)

        # Array type
        if self.match(TokenType.ARRAY):
            self.expect(TokenType.LEFT_PAREN)
            index_subtypes = []

            # Parse index types/ranges
            index_subtypes.append(self.parse_discrete_range_or_subtype())
            while self.match(TokenType.COMMA):
                index_subtypes.append(self.parse_discrete_range_or_subtype())

            self.expect(TokenType.RIGHT_PAREN)
            self.expect(TokenType.OF)
            component_type = self.parse_name()

            return ArrayTypeDef(index_subtypes=index_subtypes, component_type=component_type)

        # Record type
        if self.match(TokenType.RECORD):
            components = []
            variant_part = None

            while not self.check(TokenType.END):
                if self.match(TokenType.CASE):
                    variant_part = self.parse_variant_part()
                    break
                else:
                    comp = self.parse_component_declaration()
                    components.append(comp)

            self.expect(TokenType.END)
            self.expect(TokenType.RECORD)

            return RecordTypeDef(components=components, variant_part=variant_part)

        # Access type
        if self.match(TokenType.ACCESS):
            is_all = self.match(TokenType.ALL)
            is_constant = self.match(TokenType.CONSTANT)
            designated_type = self.parse_name()
            return AccessTypeDef(is_access_all=is_all, is_access_constant=is_constant, designated_type=designated_type)

        # Derived type
        if self.match(TokenType.NEW):
            parent_type = self.parse_name()
            record_extension = None

            if self.match(TokenType.WITH):
                if self.match(TokenType.RECORD):
                    components = []
                    while not self.check(TokenType.END):
                        components.append(self.parse_component_declaration())
                    self.expect(TokenType.END)
                    self.expect(TokenType.RECORD)
                    record_extension = RecordTypeDef(components=components)

            return DerivedTypeDef(parent_type=parent_type, record_extension=record_extension)

        # Private type
        if self.match(TokenType.PRIVATE):
            return PrivateTypeDef()

        # Interface type
        if self.match(TokenType.INTERFACE):
            return InterfaceTypeDef()

        raise ParseError("Expected type definition", self.current)

    def parse_component_declaration(self) -> ComponentDecl:
        """Parse record component declaration."""
        start = self.current
        names = [self.expect_identifier()]

        while self.match(TokenType.COMMA):
            names.append(self.expect_identifier())

        self.expect(TokenType.COLON)
        type_mark = self.parse_name()

        default_value = None
        if self.match(TokenType.ASSIGN):
            default_value = self.parse_expression()

        self.expect(TokenType.SEMICOLON)

        return ComponentDecl(names=names, type_mark=type_mark, default_value=default_value, span=self.make_span(start))

    def parse_variant_part(self) -> VariantPart:
        """Parse variant part of record."""
        discriminant = self.expect_identifier()
        self.expect(TokenType.IS)

        variants = []
        while self.match(TokenType.WHEN):
            choices = self.parse_choice_list()
            self.expect(TokenType.ARROW)

            components = []
            while not self.check(TokenType.WHEN, TokenType.END):
                components.append(self.parse_component_declaration())

            variants.append(Variant(choices=choices, components=components))

        return VariantPart(discriminant=discriminant, variants=variants)

    def parse_discriminant_specifications(self) -> list[DiscriminantSpec]:
        """Parse discriminant specifications."""
        specs = []

        names = [self.expect_identifier()]
        while self.match(TokenType.COMMA):
            names.append(self.expect_identifier())

        self.expect(TokenType.COLON)

        is_access = self.match(TokenType.ACCESS)
        type_mark = self.parse_name()

        default_value = None
        if self.match(TokenType.ASSIGN):
            default_value = self.parse_expression()

        specs.append(DiscriminantSpec(names=names, type_mark=type_mark, default_value=default_value, is_access=is_access))

        return specs

    def parse_subtype_declaration(self) -> SubtypeDecl:
        """Parse subtype declaration."""
        start = self.current
        name = self.expect_identifier()
        self.expect(TokenType.IS)
        subtype_indication = self.parse_subtype_indication()
        self.expect(TokenType.SEMICOLON)

        return SubtypeDecl(name=name, subtype_indication=subtype_indication, span=self.make_span(start))

    def parse_subtype_indication(self) -> SubtypeIndication:
        """Parse subtype indication."""
        type_mark = self.parse_name()
        constraint = None

        # Parse constraint if present
        if self.check(TokenType.RANGE):
            self.advance()
            low = self.parse_expression()
            self.expect(TokenType.DOUBLE_DOT)
            high = self.parse_expression()
            constraint = RangeConstraint(range_expr=RangeExpr(low=low, high=high))

        return SubtypeIndication(type_mark=type_mark, constraint=constraint)

    def parse_object_declaration(self) -> ObjectDecl:
        """Parse object (variable/constant) declaration."""
        start = self.current
        names = [self.expect_identifier()]

        while self.match(TokenType.COMMA):
            names.append(self.expect_identifier())

        self.expect(TokenType.COLON)

        is_constant = self.match(TokenType.CONSTANT)
        is_aliased = self.match(TokenType.ALIASED)

        type_mark = None
        if not self.check(TokenType.ASSIGN):
            type_mark = self.parse_subtype_indication()

        init_expr = None
        if self.match(TokenType.ASSIGN):
            init_expr = self.parse_expression()

        self.expect(TokenType.SEMICOLON)

        return ObjectDecl(
            names=names,
            type_mark=type_mark,
            is_constant=is_constant,
            is_aliased=is_aliased,
            init_expr=init_expr,
            span=self.make_span(start),
        )

    def parse_subprogram(self) -> SubprogramDecl | SubprogramBody:
        """Parse subprogram declaration or body."""
        spec = self.parse_subprogram_specification()

        # Check if it's a body or just a declaration
        if self.match(TokenType.IS):
            # It's a body
            if self.match(TokenType.ABSTRACT):
                self.expect(TokenType.SEMICOLON)
                spec.is_abstract = True
                return spec

            # Parse body
            declarations = self.parse_declarative_part()

            self.expect(TokenType.BEGIN)
            statements = self.parse_statement_sequence()

            handlers = []
            if self.match(TokenType.EXCEPTION):
                handlers = self.parse_exception_handlers()

            self.expect(TokenType.END)
            if self.check(TokenType.IDENTIFIER):
                self.advance()  # Optional subprogram name
            self.expect(TokenType.SEMICOLON)

            return SubprogramBody(
                spec=spec,
                declarations=declarations,
                statements=statements,
                handled_exception_handlers=handlers,
            )
        else:
            # Just a declaration
            self.expect(TokenType.SEMICOLON)
            return spec

    def parse_subprogram_specification(self) -> SubprogramDecl:
        """Parse subprogram specification."""
        start = self.current

        is_overriding = False
        is_not_overriding = False

        if self.match(TokenType.OVERRIDING):
            is_overriding = True
        elif self.check(TokenType.NOT) and self.peek(1).type == TokenType.OVERRIDING:
            self.advance()
            self.advance()
            is_not_overriding = True

        is_function = self.match(TokenType.FUNCTION)
        if not is_function:
            self.expect(TokenType.PROCEDURE)

        name = self.expect_identifier()

        parameters = []
        if self.match(TokenType.LEFT_PAREN):
            parameters = self.parse_parameter_specifications()
            self.expect(TokenType.RIGHT_PAREN)

        return_type = None
        if is_function:
            self.expect(TokenType.RETURN)
            return_type = self.parse_name()

        return SubprogramDecl(
            name=name,
            is_function=is_function,
            parameters=parameters,
            return_type=return_type,
            is_overriding=is_overriding,
            is_not_overriding=is_not_overriding,
            span=self.make_span(start),
        )

    def parse_parameter_specifications(self) -> list[ParameterSpec]:
        """Parse parameter specifications."""
        params = []

        while not self.check(TokenType.RIGHT_PAREN):
            names = [self.expect_identifier()]
            while self.match(TokenType.COMMA):
                if self.check(TokenType.COLON):
                    break
                names.append(self.expect_identifier())

            self.expect(TokenType.COLON)

            # Parse mode
            mode = "in"
            if self.match(TokenType.IN):
                if self.match(TokenType.OUT):
                    mode = "in out"
                else:
                    mode = "in"
            elif self.match(TokenType.OUT):
                mode = "out"
            elif self.match(TokenType.ACCESS):
                mode = "access"

            is_aliased = self.match(TokenType.ALIASED)
            type_mark = self.parse_name()

            default_value = None
            if self.match(TokenType.ASSIGN):
                default_value = self.parse_expression()

            params.append(
                ParameterSpec(
                    names=names,
                    mode=mode,
                    type_mark=type_mark,
                    default_value=default_value,
                    is_aliased=is_aliased,
                )
            )

            if not self.match(TokenType.SEMICOLON):
                break

        return params

    def parse_package(self) -> PackageDecl | PackageBody:
        """Parse package declaration or body."""
        start = self.current
        self.expect(TokenType.PACKAGE)

        is_body = self.match(TokenType.BODY)
        name = self.expect_identifier()

        if is_body:
            return self.parse_package_body(name, start)
        else:
            return self.parse_package_specification(name, start)

    def parse_package_specification(self, name: str, start: Token) -> PackageDecl | GenericInstantiation:
        """Parse package specification or instantiation."""
        self.expect(TokenType.IS)

        # Check for generic instantiation: package X is new Generic_Pkg(...)
        if self.match(TokenType.NEW):
            return self.parse_generic_instantiation("package", name, start)

        declarations = self.parse_declarative_part()

        private_declarations = []
        if self.match(TokenType.PRIVATE):
            private_declarations = self.parse_declarative_part()

        self.expect(TokenType.END)
        if self.check(TokenType.IDENTIFIER):
            self.advance()  # Optional package name
        self.expect(TokenType.SEMICOLON)

        return PackageDecl(
            name=name, declarations=declarations, private_declarations=private_declarations, span=self.make_span(start)
        )

    def parse_package_body(self, name: str, start: Token) -> PackageBody:
        """Parse package body."""
        self.expect(TokenType.IS)

        declarations = self.parse_declarative_part()

        statements = []
        handlers = []
        if self.match(TokenType.BEGIN):
            statements = self.parse_statement_sequence()

            if self.match(TokenType.EXCEPTION):
                handlers = self.parse_exception_handlers()

        self.expect(TokenType.END)
        if self.check(TokenType.IDENTIFIER):
            self.advance()
        self.expect(TokenType.SEMICOLON)

        return PackageBody(
            name=name,
            declarations=declarations,
            statements=statements,
            handled_exception_handlers=handlers,
            span=self.make_span(start),
        )

    def parse_generic_declaration(self) -> Decl:
        """Parse generic declaration."""
        start = self.current
        self.expect(TokenType.GENERIC)

        # Parse generic formals
        formals = []
        while not self.check(TokenType.PACKAGE, TokenType.PROCEDURE, TokenType.FUNCTION):
            formal = self.parse_generic_formal()
            formals.append(formal)

        # Parse generic unit
        if self.match(TokenType.PACKAGE):
            name = self.expect_identifier()
            pkg = self.parse_package_specification(name, start)
            pkg.generic_formals = formals
            return pkg
        else:
            subprog = self.parse_subprogram_specification()
            self.expect(TokenType.SEMICOLON)
            # Would need GenericSubprogramDecl wrapper
            return subprog

    def parse_generic_formal(self) -> GenericFormal:
        """Parse generic formal parameter."""
        start = self.current

        if self.match(TokenType.TYPE):
            name = self.expect_identifier()
            self.expect(TokenType.IS)

            # Parse generic type definition
            # Syntax: is [tagged] private | is (<>) | is array ...
            is_tagged = self.match(TokenType.TAGGED)
            if self.match(TokenType.PRIVATE):
                self.expect(TokenType.SEMICOLON)
                return GenericTypeDecl(name=name, is_tagged=is_tagged)
            elif is_tagged:
                # "is tagged" must be followed by "private"
                raise ParseError("Expected 'private' after 'tagged'", self.current)
            else:
                # Other type definitions
                type_def = self.parse_type_definition()
                self.expect(TokenType.SEMICOLON)
                return GenericTypeDecl(name=name, definition=type_def)

        # Generic object formal: identifier : [mode] type [:= default]
        if self.check(TokenType.IDENTIFIER):
            obj_name = self.expect_identifier()
            self.expect(TokenType.COLON)

            # Parse mode (in, out, in out)
            mode = "in"  # Default
            if self.match(TokenType.IN):
                if self.match(TokenType.OUT):
                    mode = "in out"
            elif self.match(TokenType.OUT):
                mode = "out"

            type_ref = self.parse_name()

            default_value = None
            if self.match(TokenType.ASSIGN):
                default_value = self.parse_expression()

            self.expect(TokenType.SEMICOLON)
            return GenericObjectDecl(
                name=obj_name, mode=mode, type_ref=type_ref, default_value=default_value
            )

        # Generic subprogram formal: with procedure/function ...
        if self.match(TokenType.WITH):
            if self.match(TokenType.PROCEDURE):
                name = self.expect_identifier()
                params = []
                if self.match(TokenType.LEFT_PAREN):
                    params = self.parse_parameter_specifications()
                    self.expect(TokenType.RIGHT_PAREN)

                # Check for "is <>"
                is_box = False
                if self.match(TokenType.IS):
                    if self.match(TokenType.BOX):
                        is_box = True

                self.expect(TokenType.SEMICOLON)
                return GenericSubprogramDecl(
                    name=name, kind="procedure", params=params, is_box=is_box
                )

            elif self.match(TokenType.FUNCTION):
                # Name can be identifier or operator string like "="
                if self.check(TokenType.STRING_LITERAL):
                    name = self.advance().value  # Operator name as string
                else:
                    name = self.expect_identifier()
                params = []
                if self.match(TokenType.LEFT_PAREN):
                    params = self.parse_parameter_specifications()
                    self.expect(TokenType.RIGHT_PAREN)

                self.expect(TokenType.RETURN)
                return_type = self.parse_name()

                # Check for "is <>"
                is_box = False
                if self.match(TokenType.IS):
                    if self.match(TokenType.BOX):
                        is_box = True

                self.expect(TokenType.SEMICOLON)
                return GenericSubprogramDecl(
                    name=name, kind="function", params=params, return_type=return_type, is_box=is_box
                )

            elif self.match(TokenType.PACKAGE):
                # Generic package formal: with package X is new Generic_Pkg(<>)
                name = self.expect_identifier()
                self.expect(TokenType.IS)
                self.expect(TokenType.NEW)
                generic_ref = self.parse_name()
                self.expect(TokenType.LEFT_PAREN)
                self.expect(TokenType.BOX)  # (<>)
                self.expect(TokenType.RIGHT_PAREN)
                self.expect(TokenType.SEMICOLON)
                return GenericPackageDecl(name=name, generic_ref=generic_ref)

        raise ParseError("Generic formal parsing incomplete", self.current)

    def parse_generic_instantiation(self, kind: str, name: str, start: Token) -> GenericInstantiation:
        """Parse generic instantiation: is new Generic_Name(actuals)."""
        # Parse just the generic unit name (may be qualified like Pkg.Generic_Unit)
        generic_name = self.parse_qualified_name()

        actual_parameters = []
        if self.match(TokenType.LEFT_PAREN):
            # Parse actual parameters
            while True:
                # Could be named: Formal => Actual or positional
                if self.check(TokenType.IDENTIFIER):
                    # Look ahead for =>
                    saved_pos = self.pos
                    saved_current = self.current
                    param_name = self.expect_identifier()
                    if self.match(TokenType.ARROW):
                        # Named parameter
                        actual = self.parse_expression()
                        actual_parameters.append(
                            ActualParameter(name=param_name, value=actual)
                        )
                    else:
                        # Positional - rewind and parse as expression
                        self.pos = saved_pos
                        self.current = saved_current
                        actual = self.parse_expression()
                        actual_parameters.append(
                            ActualParameter(value=actual)
                        )
                else:
                    actual = self.parse_expression()
                    actual_parameters.append(
                        ActualParameter(value=actual)
                    )

                if not self.match(TokenType.COMMA):
                    break

            self.expect(TokenType.RIGHT_PAREN)

        self.expect(TokenType.SEMICOLON)

        return GenericInstantiation(
            kind=kind,
            name=name,
            generic_name=generic_name,
            actual_parameters=actual_parameters,
            span=self.make_span(start),
        )

    def parse_task_declaration(self) -> TaskTypeDecl:
        """Parse task type declaration."""
        start = self.current
        self.expect(TokenType.TASK)

        is_type = self.match(TokenType.TYPE)
        name = self.expect_identifier()

        # Simplified task parsing
        self.expect(TokenType.SEMICOLON)

        return TaskTypeDecl(name=name, span=self.make_span(start))

    def parse_protected_declaration(self) -> ProtectedTypeDecl:
        """Parse protected type declaration."""
        start = self.current
        self.expect(TokenType.PROTECTED)

        is_type = self.match(TokenType.TYPE)
        name = self.expect_identifier()

        # Simplified protected parsing
        self.expect(TokenType.SEMICOLON)

        return ProtectedTypeDecl(name=name, span=self.make_span(start))


def parse(source: str, filename: str = "<input>") -> Program:
    """Convenience function to parse Ada source code."""
    from .lexer import lex

    tokens = lex(source, filename)
    parser = Parser(tokens)
    return parser.parse()
