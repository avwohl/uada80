"""
Recursive descent parser for Ada.

Implements a complete parser for Ada 2012 based on the Ada Reference Manual grammar.
"""

from typing import Optional
from .lexer import Token, TokenType
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

    def peek_next_type(self) -> TokenType:
        """Peek at the type of the next token."""
        return self.peek(1).type

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
        # Skip tokens until we find something that looks like a new declaration
        while not self.check(TokenType.EOF):
            # Check for tokens that typically start new declarations/statements
            if self.check(
                TokenType.PROCEDURE,
                TokenType.FUNCTION,
                TokenType.PACKAGE,
                TokenType.TYPE,
                TokenType.SUBTYPE,
                TokenType.BEGIN,
                TokenType.END,
                TokenType.SEPARATE,
                TokenType.TASK,
                TokenType.PROTECTED,
                TokenType.GENERIC,
                TokenType.ENTRY,
                TokenType.FOR,  # representation clause
                TokenType.PRAGMA,
                TokenType.USE,
                TokenType.PRIVATE,
            ):
                return
            # Skip this token
            prev_type = self.current.type
            self.advance()
            # Stop after a semicolon (likely end of problematic construct)
            if prev_type == TokenType.SEMICOLON:
                return

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

    def _looks_like_entry_family_index(self) -> bool:
        """Check if current position looks like an entry family index rather than parameters.

        Entry family index forms:
            (1 .. 10)      - starts with integer literal
            (Index_Type)   - single identifier without colon after
            (A'Range)      - attribute reference

        Parameter forms:
            (I : Type)     - identifier followed by colon
            (A, B : Type)  - multiple identifiers followed by colon
        """
        # If it starts with a literal, it's definitely a family index
        if self.check(TokenType.INTEGER_LITERAL, TokenType.REAL_LITERAL):
            return True

        # If it's an identifier, look ahead to see if there's a colon (parameter)
        # or something else (family index like a type name)
        if self.check(TokenType.IDENTIFIER):
            # Save position for lookahead
            saved_pos = self.pos
            self.advance()  # skip first identifier

            # Skip over dots for qualified names (Pkg.Type)
            while self.match(TokenType.DOT):
                if self.check(TokenType.IDENTIFIER):
                    self.advance()
                else:
                    break

            # Check what follows
            is_family = False
            if self.check(TokenType.RIGHT_PAREN):
                # Just "(Type_Name)" - could be either, assume family index
                is_family = True
            elif self.check(TokenType.DOUBLE_DOT):
                # "(Low .. High)" - definitely family index
                is_family = True
            elif self.check(TokenType.APOSTROPHE):
                # "(Type'Range)" - attribute, family index
                is_family = True
            elif self.check(TokenType.COLON):
                # "(Name : Type)" - parameter
                is_family = False
            elif self.check(TokenType.COMMA):
                # "(A, B : Type)" - multiple parameter names
                is_family = False
            else:
                # Something else, default to family index
                is_family = True

            # Restore position
            self.pos = saved_pos
            self.current = self.tokens[self.pos]
            return is_family

        return False

    # ========================================================================
    # Top-Level Parsing
    # ========================================================================

    def parse(self) -> Program:
        """Parse a complete Ada program (one or more compilation units)."""
        units = []
        while not self.check(TokenType.EOF):
            start_pos = self.pos
            try:
                unit = self.parse_compilation_unit()
                units.append(unit)
            except ParseError as e:
                print(f"Parse error: {e}")
                self.synchronize()
                # If synchronize didn't advance, force advance to avoid infinite loop
                if self.pos == start_pos:
                    self.advance()

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
        """Parse context clause (with, use clauses, and context pragmas).

        In Ada, pragmas like ELABORATE, ELABORATE_ALL, and ELABORATE_BODY
        can appear in the context clause area before a library unit.
        """
        clauses: list[WithClause | UseClause] = []

        while self.check(TokenType.WITH, TokenType.USE, TokenType.PRAGMA):
            if self.match(TokenType.WITH):
                clauses.append(self.parse_with_clause())
            elif self.match(TokenType.USE):
                clauses.append(self.parse_use_clause())
            elif self.match(TokenType.PRAGMA):
                # Skip context pragmas (ELABORATE, ELABORATE_ALL, etc.)
                # They don't affect visibility, only elaboration order
                self.expect_identifier()  # pragma name
                if self.match(TokenType.LEFT_PAREN):
                    # Skip pragma arguments
                    depth = 1
                    while depth > 0 and not self.check(TokenType.EOF):
                        if self.match(TokenType.LEFT_PAREN):
                            depth += 1
                        elif self.match(TokenType.RIGHT_PAREN):
                            depth -= 1
                        else:
                            self.advance()
                self.expect(TokenType.SEMICOLON)

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
        is_all = self.match(TokenType.ALL)
        is_type = self.match(TokenType.TYPE)

        # 'use all' must be followed by 'type'
        if is_all and not is_type:
            raise ParseError("Expected 'type' after 'use all'", self.current)

        names = [self.parse_name()]
        while self.match(TokenType.COMMA):
            names.append(self.parse_name())

        self.expect(TokenType.SEMICOLON)

        return UseClause(names=names, is_type=is_type, is_all=is_all, span=self.make_span(start))

    def parse_library_item(self) -> Decl:
        """Parse a library item (package, subprogram, generic, subunit, etc.)."""
        # Check for separate subunit: SEPARATE (parent) body
        if self.check(TokenType.SEPARATE):
            return self.parse_subunit()

        # Check for generic
        if self.check(TokenType.GENERIC):
            return self.parse_generic_declaration()

        # Package or subprogram
        if self.check(TokenType.PACKAGE):
            return self.parse_package()
        elif self.check(TokenType.PROCEDURE, TokenType.FUNCTION):
            return self.parse_subprogram()
        elif self.check(TokenType.TASK):
            return self.parse_task_declaration()
        elif self.check(TokenType.PROTECTED):
            return self.parse_protected_declaration()
        else:
            raise ParseError("Expected package, subprogram, generic, or separate", self.current)

    def parse_subunit(self) -> Subunit:
        """Parse a separate subunit: SEPARATE (parent_name) body."""
        start = self.current
        self.expect(TokenType.SEPARATE)
        self.expect(TokenType.LEFT_PAREN)
        parent_unit = self.parse_name()
        self.expect(TokenType.RIGHT_PAREN)

        # Parse the body (procedure, function, package, task, or protected body)
        if self.check(TokenType.PROCEDURE, TokenType.FUNCTION):
            body = self.parse_subprogram()
        elif self.check(TokenType.PACKAGE):
            body_start = self.current
            self.expect(TokenType.PACKAGE)
            self.expect(TokenType.BODY)
            name = self.expect_identifier()
            body = self.parse_package_body(name, body_start)
        elif self.check(TokenType.TASK):
            body_start = self.current
            self.expect(TokenType.TASK)
            self.expect(TokenType.BODY)
            name = self.expect_identifier()
            body = self.parse_task_body_impl(name, body_start)
        elif self.check(TokenType.PROTECTED):
            body_start = self.current
            self.expect(TokenType.PROTECTED)
            self.expect(TokenType.BODY)
            name = self.expect_identifier()
            body = self.parse_protected_body_impl(name, body_start)
        else:
            raise ParseError("Expected procedure, function, package body, task body, or protected body after SEPARATE", self.current)

        return Subunit(parent_unit=parent_unit, body=body, span=self.make_span(start))

    # ========================================================================
    # Names and Expressions
    # ========================================================================

    def parse_dotted_name(self) -> str:
        """
        Parse a dotted name and return it as a string.

        Used for package names which can be child packages like Ada.Text_IO.
        """
        parts = [self.expect_identifier()]
        while self.match(TokenType.DOT):
            parts.append(self.expect_identifier())
        return ".".join(parts)

    def name_to_string(self, name: Expr) -> str:
        """Convert a name expression (Identifier or SelectedName) to a string."""
        if isinstance(name, Identifier):
            return name.name
        elif isinstance(name, SelectedName):
            return self.name_to_string(name.prefix) + "." + name.selector
        else:
            return str(name)

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
            name = SelectedName(
                prefix=name, selector=component, span=self.make_span(start)
            )

        return name

    def parse_name(self) -> Expr:
        """
        Parse a name (identifier, selected name, indexed component, etc.).

        Ada grammar for names is quite complex, handling:
        - Simple identifiers
        - Operator symbols ("+" can be a name in renaming contexts)
        - Selected components (Package.Entity)
        - Indexed components (Array(I))
        - Slices (Array(1..10))
        - Attribute references (X'First)
        - Function calls (which look like indexed components)
        """
        start = self.current

        # Handle operator symbol as name (e.g., function "+" renames Other."+";)
        if self.check(TokenType.STRING_LITERAL):
            op_name = self.current.value
            self.advance()
            name: Expr = Identifier(name=op_name, span=self.make_span(start))
        else:
            name = Identifier(name=self.expect_identifier(), span=self.make_span(start))

        # Handle suffixes (dot, apostrophe, parentheses)
        while True:
            if self.match(TokenType.DOT):
                # Selected component or dereference (.all)
                if self.match(TokenType.ALL):
                    # Dereference: P.all
                    name = Dereference(prefix=name, span=self.make_span(start))
                elif self.check(TokenType.STRING_LITERAL):
                    # Operator name: Package."="
                    selector = self.current.value
                    self.advance()
                    name = SelectedName(prefix=name, selector=selector, span=self.make_span(start))
                else:
                    selector = self.expect_identifier()
                    name = SelectedName(prefix=name, selector=selector, span=self.make_span(start))

            elif self.match(TokenType.APOSTROPHE):
                # Could be attribute reference (X'First) or qualified expression (Integer'(100))
                if self.check(TokenType.LEFT_PAREN):
                    # Qualified expression: Type'(Expression) or Type'(Aggregate)
                    self.advance()  # consume LEFT_PAREN
                    # Parse as aggregate components to handle both single expr and aggregates
                    if self.check(TokenType.RIGHT_PAREN):
                        # Empty aggregate - null record
                        expr = Aggregate(components=[], span=self.make_span(start))
                    else:
                        components = self.parse_aggregate_components()
                        if len(components) == 1 and components[0].choices is None:
                            # Single positional component - extract as expression
                            expr = components[0].value
                        else:
                            # Multiple components or named - it's an aggregate
                            expr = Aggregate(components=components, span=self.make_span(start))
                    self.expect(TokenType.RIGHT_PAREN)
                    name = QualifiedExpr(type_mark=name, expr=expr, span=self.make_span(start))
                else:
                    # Attribute reference
                    # Attribute names can be identifiers or certain keywords like ACCESS, RANGE, etc.
                    attr_name = self._parse_attribute_designator()
                    args = []
                    if self.match(TokenType.LEFT_PAREN):
                        args = self.parse_expression_list()
                        self.expect(TokenType.RIGHT_PAREN)
                    name = AttributeReference(
                        prefix=name, attribute=attr_name, args=args, span=self.make_span(start)
                    )

            elif self.match(TokenType.LEFT_PAREN):
                # Either indexed component, slice, function call, or named association
                args = self.parse_actual_parameter_list()
                self.expect(TokenType.RIGHT_PAREN)

                # If we have a single argument that's a range, treat as slice
                if (
                    len(args) == 1
                    and args[0].name is None
                    and isinstance(args[0].value, RangeExpr)
                ):
                    name = Slice(
                        prefix=name, range_expr=args[0].value, span=self.make_span(start)
                    )
                else:
                    # Use IndexedComponent for now; semantic analysis will resolve
                    indices = [arg.value for arg in args]
                    name = IndexedComponent(prefix=name, indices=indices,
                                            actual_params=args, span=self.make_span(start))

            else:
                # No more suffixes
                break

        return name

    def _parse_attribute_designator(self) -> str:
        """Parse an attribute designator.

        Attribute names can be identifiers or certain keywords that are also
        valid attribute names (ACCESS, RANGE, DELTA, DIGITS, etc.).
        """
        # Keywords that are valid attribute names
        keyword_attributes = {
            TokenType.ACCESS: "Access",
            TokenType.RANGE: "Range",
            TokenType.DELTA: "Delta",
            TokenType.DIGITS: "Digits",
            TokenType.MOD: "Mod",
            TokenType.ABORT: "Abort",  # For Exception_Name'Abort
        }

        if self.current.type in keyword_attributes:
            attr = keyword_attributes[self.current.type]
            self.advance()
            return attr
        else:
            return self.expect_identifier()

    def _parse_enumeration_literal(self) -> str:
        """Parse an enumeration literal (identifier or character literal).

        Ada enumeration types can contain both identifiers and character literals:
            type Mixed is (Red, 'R', Green, 'G', Blue, 'B');
        """
        if self.check(TokenType.CHARACTER_LITERAL):
            token = self.advance()
            return token.value  # Return the character literal including quotes
        else:
            return self.expect_identifier()

    def parse_actual_parameter_list(self) -> list[ActualParameter]:
        """Parse actual parameter list, handling both positional and named parameters.

        Syntax:
            positional: expr
            named: name => expr
        """
        params: list[ActualParameter] = []

        if self.check(TokenType.RIGHT_PAREN):
            return params

        params.append(self.parse_actual_parameter())
        while self.match(TokenType.COMMA):
            params.append(self.parse_actual_parameter())

        return params

    def parse_actual_parameter(self) -> ActualParameter:
        """Parse a single actual parameter (positional or named)."""
        # Check if this is a named association: identifier => value or identifier | identifier => value
        if self.check(TokenType.IDENTIFIER):
            # Look ahead to see if this is a named association
            # Could be: A => value, or A | B => value, or A | B | C => value
            save_pos = self.pos
            names = [self.advance().value]
            while self.match(TokenType.PIPE):
                if not self.check(TokenType.IDENTIFIER):
                    # Not a valid choice list, restore and parse as expression
                    self.pos = save_pos
                    self.current = self.tokens[self.pos]
                    break
                names.append(self.advance().value)

            if self.check(TokenType.ARROW):
                self.advance()  # consume =>
                value = self.parse_expression()
                # Return with first name (semantic analysis will handle multiple choices)
                return ActualParameter(name="|".join(names), value=value)
            else:
                # Not a named association, restore position and parse as expression
                self.pos = save_pos
                self.current = self.tokens[self.pos]

        # Check for 'others => expr' syntax in aggregates
        if self.check(TokenType.OTHERS) and self.peek(1).type == TokenType.ARROW:
            self.advance()  # consume 'others'
            self.advance()  # consume '=>'
            value = self.parse_expression()
            return ActualParameter(name="others", value=value)

        # Positional parameter
        value = self.parse_expression()

        # Check for range expression (for slices)
        if self.match(TokenType.DOUBLE_DOT):
            high = self.parse_expression()
            value = RangeExpr(low=value, high=high, span=None)

        return ActualParameter(name=None, value=value)

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
            # Membership test - supports X in A | B | C (Ada 2012)
            choices = self._parse_membership_choices()
            return MembershipTest(expr=left, is_not=False, choices=choices, span=self.make_span(start))
        elif self.check(TokenType.NOT) and self.peek(1).type == TokenType.IN:
            self.advance()  # not
            self.advance()  # in
            # Membership test with NOT - supports X not in A | B | C (Ada 2012)
            choices = self._parse_membership_choices()
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

    def _parse_membership_choices(self) -> list[Choice]:
        """Parse membership test choices (X in A | B | C)."""
        choices: list[Choice] = []

        while True:
            # Parse a single choice: value, range, or type name
            expr = self.parse_additive()
            if self.match(TokenType.DOUBLE_DOT):
                # Range: A .. B
                high = self.parse_additive()
                range_expr = RangeExpr(low=expr, high=high)
                choices.append(RangeChoice(range_expr=range_expr))
            else:
                # Simple expression or type name
                choices.append(ExprChoice(expr=expr))

            # Check for more choices
            if not self.match(TokenType.PIPE):
                break

        return choices

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
            value = self.parse_real_literal(text)
            self.advance()
            return RealLiteral(value=value, text=text, span=self.make_span(start))

        if self.check(TokenType.STRING_LITERAL):
            value = self.current.value
            self.advance()
            # Check if this is an operator symbol used as a function call: "+"(A, B)
            if self.check(TokenType.LEFT_PAREN):
                # It's an operator call - parse as name with function call suffix
                name: Expr = Identifier(name=value, span=self.make_span(start))
                self.advance()  # consume LEFT_PAREN
                args = self.parse_actual_parameter_list()
                self.expect(TokenType.RIGHT_PAREN)
                indices = [arg.value for arg in args]
                name = IndexedComponent(prefix=name, indices=indices, span=self.make_span(start))
                # Continue parsing additional suffixes (e.g., "(5)" for indexing result)
                while True:
                    if self.match(TokenType.LEFT_PAREN):
                        args = self.parse_actual_parameter_list()
                        self.expect(TokenType.RIGHT_PAREN)
                        if len(args) == 1 and args[0].name is None and isinstance(args[0].value, RangeExpr):
                            name = Slice(prefix=name, range_expr=args[0].value, span=self.make_span(start))
                        else:
                            name = IndexedComponent(prefix=name, indices=[arg.value for arg in args], span=self.make_span(start))
                    elif self.match(TokenType.APOSTROPHE):
                        attr_name = self._parse_attribute_designator()
                        attr_args = []
                        if self.match(TokenType.LEFT_PAREN):
                            attr_args = self.parse_expression_list()
                            self.expect(TokenType.RIGHT_PAREN)
                        name = AttributeReference(prefix=name, attribute=attr_name, args=attr_args, span=self.make_span(start))
                    else:
                        break
                return name
            return StringLiteral(value=value, span=self.make_span(start))

        if self.check(TokenType.CHARACTER_LITERAL):
            value = self.current.value
            self.advance()
            return CharacterLiteral(value=value, span=self.make_span(start))

        if self.match(TokenType.NULL):
            return NullLiteral(span=self.make_span(start))

        # Ada 2012 raise expression: raise Exception [with "message"]
        if self.match(TokenType.RAISE):
            exception_name = self.parse_name()
            message = None
            if self.match(TokenType.WITH):
                message = self.parse_expression()
            return RaiseExpr(exception_name=exception_name, message=message, span=self.make_span(start))

        # Ada 2022 target name: @ refers to target of assignment
        if self.match(TokenType.AT_SIGN):
            return TargetName(span=self.make_span(start))

        # Ada 2022 container aggregate: [...]
        if self.match(TokenType.LEFT_BRACKET):
            components = self.parse_aggregate_components()
            self.expect(TokenType.RIGHT_BRACKET)
            result: Expr = ContainerAggregate(components=components, span=self.make_span(start))
            # Allow attribute suffixes on container aggregates (e.g., [...]'Reduce)
            return self.parse_aggregate_attribute_suffix(result, start)

        # Parenthesized expression, aggregate, conditional expr, or quantified expr
        if self.match(TokenType.LEFT_PAREN):
            # Ada 2022 declare expression: (declare ... begin Expr)
            if self.check(TokenType.DECLARE):
                return self.parse_declare_expr(start)

            # Ada 2012 conditional expression: (if Cond then Expr ...)
            if self.check(TokenType.IF):
                return self.parse_conditional_expr(start)

            # Ada 2012 quantified expression: (for all/some ...)
            # Distinguish from iterated component association: (for Name in/of ...)
            if self.check(TokenType.FOR):
                next_tok = self.peek(1).type
                if next_tok == TokenType.ALL or next_tok == TokenType.SOME:
                    return self.parse_quantified_expr(start)
                else:
                    # Iterated component association as aggregate
                    components = self.parse_aggregate_components()
                    self.expect(TokenType.RIGHT_PAREN)
                    agg = Aggregate(components=components, span=self.make_span(start))
                    return self.parse_aggregate_attribute_suffix(agg, start)

            # Ada 2012 case expression: (case Selector is ...)
            if self.check(TokenType.CASE):
                return self.parse_case_expr(start)

            # Could be aggregate or parenthesized expression
            # Check for 'others' which definitely means aggregate
            if self.check(TokenType.OTHERS):
                components = self.parse_aggregate_components()
                self.expect(TokenType.RIGHT_PAREN)
                agg = Aggregate(components=components, span=self.make_span(start))
                return self.parse_aggregate_attribute_suffix(agg, start)

            # Parse first expression
            first_expr = self.parse_expression()

            # Ada 2022 delta aggregate: (base with delta Field => Value, ...)
            if self.check(TokenType.WITH) and self.peek(1).type == TokenType.DELTA:
                self.advance()  # consume WITH
                self.advance()  # consume DELTA
                components = self.parse_aggregate_components()
                self.expect(TokenType.RIGHT_PAREN)
                agg = DeltaAggregate(
                    base_expression=first_expr,
                    components=components,
                    span=self.make_span(start),
                )
                return self.parse_aggregate_attribute_suffix(agg, start)

            # Extension aggregate: (Base_Type with Field => Value, ...)
            # Used for record extension types in Ada 95+
            if self.check(TokenType.WITH) and self.peek(1).type != TokenType.DELTA:
                self.advance()  # consume WITH
                components = self.parse_aggregate_components()
                self.expect(TokenType.RIGHT_PAREN)
                agg = ExtensionAggregate(
                    ancestor_part=first_expr,
                    components=components,
                    span=self.make_span(start),
                )
                return self.parse_aggregate_attribute_suffix(agg, start)

            # Check what follows to determine aggregate vs parenthesized
            if self.match(TokenType.DOUBLE_DOT):
                # Range choice: (low .. high => value, ...)
                high = self.parse_expression()
                range_expr = RangeExpr(low=first_expr, high=high, span=self.make_span(start))
                choices: list[Choice] = [RangeChoice(range_expr=range_expr)]

                # Check for additional choices with pipe
                while self.match(TokenType.PIPE):
                    next_expr = self.parse_expression()
                    if self.match(TokenType.DOUBLE_DOT):
                        next_high = self.parse_expression()
                        next_range = RangeExpr(low=next_expr, high=next_high, span=self.make_span(start))
                        choices.append(RangeChoice(range_expr=next_range))
                    else:
                        choices.append(ExprChoice(expr=next_expr))

                self.expect(TokenType.ARROW)
                value = self.parse_expression()
                components = [ComponentAssociation(choices=choices, value=value)]

                while self.match(TokenType.COMMA):
                    comp = self.parse_aggregate_component()
                    components.append(comp)

                self.expect(TokenType.RIGHT_PAREN)
                agg = Aggregate(components=components, span=self.make_span(start))
                return self.parse_aggregate_attribute_suffix(agg, start)

            elif self.check(TokenType.PIPE) or self.check(TokenType.ARROW):
                # Named aggregate: (field => value, ...) or (A|B => value, ...)
                choices: list[Choice] = [ExprChoice(expr=first_expr)]

                # Check for additional choices with pipe (A|B|C => value)
                while self.match(TokenType.PIPE):
                    next_expr = self.parse_expression()
                    if self.match(TokenType.DOUBLE_DOT):
                        next_high = self.parse_expression()
                        next_range = RangeExpr(low=next_expr, high=next_high, span=self.make_span(start))
                        choices.append(RangeChoice(range_expr=next_range))
                    else:
                        choices.append(ExprChoice(expr=next_expr))

                self.expect(TokenType.ARROW)
                value = self.parse_expression()
                components = [ComponentAssociation(choices=choices, value=value)]

                while self.match(TokenType.COMMA):
                    comp = self.parse_aggregate_component()
                    components.append(comp)

                self.expect(TokenType.RIGHT_PAREN)
                agg = Aggregate(components=components, span=self.make_span(start))
                return self.parse_aggregate_attribute_suffix(agg, start)

            elif self.match(TokenType.COMMA):
                # Positional aggregate: (val1, val2, ...)
                # Ada allows mixing positional and named: (1, 2, X => 3)
                components = [ComponentAssociation(choices=[], value=first_expr)]

                while True:
                    # Use parse_aggregate_component to handle named associations
                    comp = self.parse_aggregate_component()
                    components.append(comp)
                    if not self.match(TokenType.COMMA):
                        break

                self.expect(TokenType.RIGHT_PAREN)
                agg = Aggregate(components=components, span=self.make_span(start))
                return self.parse_aggregate_attribute_suffix(agg, start)

            else:
                # Simple parenthesized expression
                self.expect(TokenType.RIGHT_PAREN)
                return Parenthesized(expr=first_expr, span=self.make_span(start))

        # Allocator (new Type)
        if self.match(TokenType.NEW):
            type_mark = self.parse_name()
            init_value = None
            # parse_name() may have consumed a qualified expression (new Integer'(42))
            # In that case, type_mark is QualifiedExpr and we need to extract the value
            if isinstance(type_mark, QualifiedExpr):
                init_value = type_mark.expr
                type_mark = type_mark.type_mark
            elif self.match(TokenType.APOSTROPHE):
                # Legacy path (shouldn't be reached normally)
                self.expect(TokenType.LEFT_PAREN)
                init_value = self.parse_expression()
                self.expect(TokenType.RIGHT_PAREN)
            return Allocator(type_mark=type_mark, initial_value=init_value, span=self.make_span(start))

        # Names (identifiers, function calls, etc.)
        if self.check(TokenType.IDENTIFIER):
            return self.parse_name()

        raise ParseError(f"Unexpected token in expression: {self.current.type.name}", self.current)

    def parse_aggregate_attribute_suffix(self, expr: Expr, start: Token) -> Expr:
        """Parse attribute suffix after an aggregate (for 'Reduce, etc.)."""
        while self.match(TokenType.APOSTROPHE):
            attr_name = self.expect_identifier()
            args: list[Expr] = []
            if self.match(TokenType.LEFT_PAREN):
                args = self.parse_expression_list()
                self.expect(TokenType.RIGHT_PAREN)
            expr = AttributeReference(
                prefix=expr, attribute=attr_name, args=args, span=self.make_span(start)
            )
        return expr

    def parse_conditional_expr(self, start: Token) -> ConditionalExpr:
        """Parse Ada 2012 conditional expression: (if Cond then Expr ...)."""
        # Already consumed LEFT_PAREN, now consume IF
        self.expect(TokenType.IF)
        condition = self.parse_expression()
        self.expect(TokenType.THEN)
        then_expr = self.parse_expression()

        # Parse elsif parts
        elsif_parts: list[tuple[Expr, Expr]] = []
        while self.match(TokenType.ELSIF):
            elsif_cond = self.parse_expression()
            self.expect(TokenType.THEN)
            elsif_expr = self.parse_expression()
            elsif_parts.append((elsif_cond, elsif_expr))

        # Parse else part (required in Ada 2012 conditional expressions)
        else_expr = None
        if self.match(TokenType.ELSE):
            else_expr = self.parse_expression()

        self.expect(TokenType.RIGHT_PAREN)
        return ConditionalExpr(
            condition=condition,
            then_expr=then_expr,
            elsif_parts=elsif_parts,
            else_expr=else_expr,
            span=self.make_span(start),
        )

    def parse_declare_expr(self, start: Token) -> DeclareExpr:
        """Parse Ada 2022 declare expression: (declare ... begin Expr).

        Syntax: (declare object_declarations begin expression)
        """
        # Already consumed LEFT_PAREN, now consume DECLARE
        self.expect(TokenType.DECLARE)

        # Parse declarations until BEGIN
        declarations: list[Decl] = []
        while not self.check(TokenType.BEGIN):
            decl = self.parse_declaration()
            if decl:
                declarations.append(decl)

        self.expect(TokenType.BEGIN)
        result_expr = self.parse_expression()
        self.expect(TokenType.RIGHT_PAREN)

        return DeclareExpr(
            declarations=declarations,
            result_expr=result_expr,
            span=self.make_span(start),
        )

    def parse_quantified_expr(self, start: Token) -> QuantifiedExpr:
        """Parse Ada 2012 quantified expression: (for all/some X in Range => Pred)."""
        # Already consumed LEFT_PAREN, now consume FOR
        self.expect(TokenType.FOR)

        # Determine quantifier: 'all' or 'some'
        if self.match(TokenType.ALL):
            is_for_all = True
        elif self.match(TokenType.SOME):
            is_for_all = False
        else:
            raise ParseError("Expected 'all' or 'some' after 'for' in quantified expression", self.current)

        # Parse loop parameter specification (similar to for loop)
        loop_var = self.expect_identifier()

        is_reverse = False
        if self.match(TokenType.IN):
            if self.match(TokenType.REVERSE):
                is_reverse = True
            iterable = self.parse_range_or_expression()
        elif self.match(TokenType.OF):
            # for all X of Array => ...
            iterable = self.parse_expression()
        else:
            raise ParseError("Expected 'in' or 'of' in quantified expression", self.current)

        iterator = IteratorSpec(name=loop_var, is_reverse=is_reverse, iterable=iterable)

        # Expect '=>' before predicate
        self.expect(TokenType.ARROW)
        predicate = self.parse_expression()

        self.expect(TokenType.RIGHT_PAREN)
        return QuantifiedExpr(
            is_for_all=is_for_all,
            iterator=iterator,
            predicate=predicate,
            span=self.make_span(start),
        )

    def parse_case_expr(self, start: Token) -> CaseExpr:
        """Parse Ada 2012 case expression: (case Selector is when ... => Expr, ...)."""
        # Already consumed LEFT_PAREN, now consume CASE
        self.expect(TokenType.CASE)
        selector = self.parse_expression()
        self.expect(TokenType.IS)

        # Parse alternatives
        alternatives: list[CaseExprAlternative] = []
        while self.match(TokenType.WHEN):
            choices = self.parse_choice_list()
            self.expect(TokenType.ARROW)
            result_expr = self.parse_expression()
            alternatives.append(CaseExprAlternative(choices=choices, result_expr=result_expr))

            # In case expressions, alternatives are separated by commas (not semicolons)
            if not self.check(TokenType.WHEN) and not self.check(TokenType.RIGHT_PAREN):
                self.match(TokenType.COMMA)

        self.expect(TokenType.RIGHT_PAREN)
        return CaseExpr(
            selector=selector,
            alternatives=alternatives,
            span=self.make_span(start),
        )

    def parse_range_or_expression(self) -> Expr:
        """Parse a range (A .. B) or a simple expression."""
        left = self.parse_additive()
        if self.match(TokenType.DOUBLE_DOT):
            right = self.parse_additive()
            return RangeExpr(low=left, high=right, span=None)
        return left

    def parse_aspect_specification(self) -> list[AspectSpecification]:
        """Parse Ada 2012 aspect specification: with Aspect [=> Expr], ...

        Returns empty list if no aspects present.
        """
        aspects: list[AspectSpecification] = []

        if not self.match(TokenType.WITH):
            return aspects

        while True:
            # Parse aspect name
            aspect_name = self.expect_identifier()
            aspect_value = None

            # Check for optional value
            if self.match(TokenType.ARROW):
                aspect_value = self.parse_expression()

            aspects.append(AspectSpecification(name=aspect_name, value=aspect_value))

            # Check for more aspects
            if not self.match(TokenType.COMMA):
                break

        return aspects

    def parse_integer_literal(self, text: str) -> int:
        """Parse Ada integer literal (handles based literals and exponents)."""
        text = text.replace("_", "")

        # Based literal: base#value#[exponent] or Ada 83 style base:value:
        delimiter = "#" if "#" in text else (":" if text.count(":") >= 2 else None)
        if delimiter:
            # Handle exponent if present - exponent comes AFTER the closing delimiter
            exp_value = 0
            # Find the last delimiter (closing delimiter of based literal)
            last_delim = text.rfind(delimiter)
            after_delim = text[last_delim + 1:] if last_delim < len(text) - 1 else ""
            if "e" in after_delim.lower():
                # Exponent is after the closing delimiter
                exp_pos = after_delim.lower().find("e")
                exp_str = after_delim[exp_pos + 1:]
                text = text[:last_delim + 1]  # Keep up to and including the closing delimiter
                exp_value = int(exp_str)

            parts = text.split(delimiter)
            base = int(parts[0])
            value_str = parts[1]
            result = int(value_str, base)
            return result * (base ** exp_value)

        # Decimal literal with exponent (e.g., 12e1 = 120, 1E3 = 1000)
        if "e" in text.lower():
            text_lower = text.lower()
            exp_pos = text_lower.find("e")
            mantissa = text[:exp_pos]
            exp_str = text[exp_pos + 1:]
            return int(mantissa) * (10 ** int(exp_str))

        # Plain decimal
        return int(text)

    def parse_real_literal(self, text: str) -> float:
        """Parse Ada real literal (handles based literals)."""
        text = text.replace("_", "")

        # Based literal: base#value#[exponent] or Ada 83 style base:value:
        delimiter = "#" if "#" in text else (":" if text.count(":") >= 2 else None)
        if delimiter:
            # Split off exponent if present - exponent comes AFTER the closing delimiter
            exp_value = 0
            # Find the last delimiter (closing delimiter of based literal)
            last_delim = text.rfind(delimiter)
            after_delim = text[last_delim + 1:] if last_delim < len(text) - 1 else ""
            if "e" in after_delim.lower():
                # Exponent is after the closing delimiter
                exp_pos = after_delim.lower().find("e")
                exp_str = after_delim[exp_pos + 1:]
                text = text[:last_delim + 1]  # Keep up to and including the closing delimiter
                exp_value = int(exp_str)

            parts = text.split(delimiter)
            base = int(parts[0])
            value_str = parts[1] if len(parts) > 1 else "0"

            # Handle fractional part
            if "." in value_str:
                int_part, frac_part = value_str.split(".")
                int_val = int(int_part, base) if int_part else 0
                frac_val = 0.0
                for i, c in enumerate(frac_part):
                    digit = int(c, base)
                    frac_val += digit / (base ** (i + 1))
                result = float(int_val) + frac_val
            else:
                result = float(int(value_str, base))

            return result * (base ** exp_value)

        # Regular decimal literal
        return float(text)

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

    def parse_aggregate_component(self) -> ComponentAssociation | IteratedComponentAssociation:
        """Parse a single aggregate component association."""
        # Check for iterated component association (Ada 2012)
        # for Name in range => expression  or  for Name of iterable => expression
        if self.check(TokenType.FOR):
            return self.parse_iterated_component()

        # Check for 'others'
        if self.match(TokenType.OTHERS):
            self.expect(TokenType.ARROW)
            value = self.parse_expression()
            return ComponentAssociation(choices=[OthersChoice()], value=value)

        # Parse choice(s) => expression or positional
        choices: list[Choice] = []
        first_expr = self.parse_expression()

        # Check for range choice: expr .. expr => value
        if self.match(TokenType.DOUBLE_DOT):
            high = self.parse_expression()
            range_choice = RangeChoice(range_expr=RangeExpr(low=first_expr, high=high))
            choices.append(range_choice)

            # Check for additional choices with pipe
            while self.match(TokenType.PIPE):
                # Parse another choice (could be expr or range)
                next_expr = self.parse_expression()
                if self.match(TokenType.DOUBLE_DOT):
                    next_high = self.parse_expression()
                    choices.append(RangeChoice(range_expr=RangeExpr(low=next_expr, high=next_high)))
                else:
                    choices.append(ExprChoice(expr=next_expr))

            self.expect(TokenType.ARROW)
            value = self.parse_expression()
            return ComponentAssociation(choices=choices, value=value)
        elif self.check(TokenType.PIPE) or self.check(TokenType.ARROW):
            # Named association with one or more choices: A | B | C => value
            choices.append(ExprChoice(expr=first_expr))
            # Handle choice list before =>
            while self.match(TokenType.PIPE):
                # Parse another choice (could be expr or range)
                next_expr = self.parse_expression()
                if self.match(TokenType.DOUBLE_DOT):
                    next_high = self.parse_expression()
                    choices.append(RangeChoice(range_expr=RangeExpr(low=next_expr, high=next_high)))
                else:
                    choices.append(ExprChoice(expr=next_expr))
            # Now expect the arrow
            self.expect(TokenType.ARROW)
            value = self.parse_expression()
            return ComponentAssociation(choices=choices, value=value)
        else:
            # Positional association
            return ComponentAssociation(choices=[], value=first_expr)

    def parse_iterated_component(self) -> IteratedComponentAssociation:
        """Parse iterated component association (Ada 2012).

        Syntax: for Name in discrete_range => expression
                for Name of iterable => expression
        """
        self.expect(TokenType.FOR)
        loop_param = self.expect_identifier()

        # Check for "in" or "of"
        if self.match(TokenType.IN):
            is_of_form = False
            iterator_spec = self.parse_discrete_range_or_name()
        elif self.match(TokenType.OF):
            is_of_form = True
            iterator_spec = self.parse_name()
        else:
            raise ParseError(
                f"Expected 'in' or 'of' in iterated component, got {self.current.type.name}",
                self.current,
            )

        self.expect(TokenType.ARROW)
        value = self.parse_expression()

        return IteratedComponentAssociation(
            loop_parameter=loop_param,
            iterator_spec=iterator_spec,
            is_of_form=is_of_form,
            value=value,
        )

    def parse_discrete_range_or_name(self) -> Expr:
        """Parse a discrete range or subtype name."""
        # This can be a range (1 .. 10) or a subtype mark (Integer)
        expr = self.parse_additive()

        # Check for ".." to see if it's a range
        if self.match(TokenType.DOUBLE_DOT):
            high = self.parse_additive()
            return RangeExpr(low=expr, high=high)

        return expr

    # ========================================================================
    # Statements
    # ========================================================================

    def parse_statement(self) -> Stmt:
        """Parse a single statement."""
        start = self.current

        # Label: <<Label>> Statement
        if self.match(TokenType.LEFT_LABEL):
            label = self.expect_identifier()
            self.expect(TokenType.RIGHT_LABEL)
            # Parse the labeled statement
            inner_stmt = self.parse_statement()
            return LabeledStmt(label=label, statement=inner_stmt, span=self.make_span(start))

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

        # Check for labeled loop: Label : loop/while/for
        # Need to look 2 tokens ahead (after IDENTIFIER COLON) to confirm
        if (self.check(TokenType.IDENTIFIER) and
            self.peek_next_type() == TokenType.COLON and
            self.peek(2).type in (TokenType.LOOP, TokenType.WHILE, TokenType.FOR)):
            label = self.expect_identifier()
            self.expect(TokenType.COLON)
            return self.parse_loop_statement(label=label)

        # Check for labeled block: Label : declare/begin
        if (self.check(TokenType.IDENTIFIER) and
            self.peek_next_type() == TokenType.COLON and
            self.peek(2).type in (TokenType.DECLARE, TokenType.BEGIN)):
            label = self.expect_identifier()
            self.expect(TokenType.COLON)
            return self.parse_block_statement(label=label)

        # Loop statement
        if self.check(TokenType.LOOP, TokenType.WHILE, TokenType.FOR):
            return self.parse_loop_statement()

        # Ada 2022 parallel constructs
        if self.check(TokenType.PARALLEL):
            return self.parse_parallel_statement()

        # Block statement (with declare) or unnamed begin block
        if self.check(TokenType.DECLARE, TokenType.BEGIN):
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
                # Use actual_params if available (preserves named parameter info)
                if name.actual_params:
                    args = name.actual_params
                else:
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

    def parse_loop_statement(
        self, label: Optional[str] = None, is_parallel: bool = False
    ) -> LoopStmt:
        """Parse loop statement."""
        start = self.current
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

        # Optional trailing loop name (must match leading label if both present)
        trailing_label = None
        if self.check(TokenType.IDENTIFIER):
            trailing_label = self.expect_identifier()
            # If we have both leading and trailing labels, trailing wins
            # (semantic will validate they match)
            if trailing_label:
                label = trailing_label

        self.expect(TokenType.SEMICOLON)

        return LoopStmt(
            iteration_scheme=iteration_scheme,
            statements=statements,
            label=label,
            is_parallel=is_parallel,
            span=self.make_span(start),
        )

    def parse_parallel_statement(self) -> LoopStmt | ParallelBlockStmt:
        """Parse Ada 2022 parallel statement.

        Syntax:
        - parallel for I in Range loop ... end loop;
        - parallel do seq1; and do seq2; end parallel;
        """
        start = self.current
        self.expect(TokenType.PARALLEL)

        # Parallel loop: parallel for ...
        if self.check(TokenType.FOR):
            return self.parse_loop_statement(is_parallel=True)

        # Parallel block: parallel do ... and do ... end parallel;
        if self.match(TokenType.DO):
            sequences: list[list[Stmt]] = []

            # Parse first sequence
            seq = self.parse_statement_sequence()
            sequences.append(seq)

            # Parse additional sequences (and do ...)
            while self.match(TokenType.AND):
                self.expect(TokenType.DO)
                seq = self.parse_statement_sequence()
                sequences.append(seq)

            self.expect(TokenType.END)
            self.expect(TokenType.PARALLEL)
            self.expect(TokenType.SEMICOLON)

            return ParallelBlockStmt(sequences=sequences, span=self.make_span(start))

        raise ParseError(
            f"Expected 'for' or 'do' after 'parallel', got {self.current.type.name}",
            self.current,
        )

    def parse_iterator_spec(self) -> IteratorSpec:
        """Parse iterator specification.

        Supports both forms:
        - for I in 1 .. 10 loop  -- Index iteration
        - for X of Array loop   -- Element iteration (Ada 2012)
        """
        name = self.expect_identifier()

        # Check for "of" (element iteration) vs "in" (index iteration)
        is_of_iterator = False
        if self.match(TokenType.OF):
            is_of_iterator = True
            is_reverse = self.match(TokenType.REVERSE)
            iterable = self.parse_expression()  # Array or container expression
        else:
            self.expect(TokenType.IN)
            is_reverse = self.match(TokenType.REVERSE)
            iterable = self.parse_discrete_range_or_subtype()  # Range or subtype

        return IteratorSpec(
            name=name,
            is_reverse=is_reverse,
            iterable=iterable,
            is_of_iterator=is_of_iterator,
        )

    def parse_discrete_range_or_subtype(self) -> Expr:
        """Parse a discrete range (1..10), subtype indication (Integer),
        or unconstrained range (Positive range <>).

        Handles:
        - Simple range: 1..10
        - Type mark: Integer
        - Subtype with range constraint: Integer range 1..10
        - Unconstrained range: Positive range <>
        """
        start = self.current
        first_expr = self.parse_additive()

        if self.match(TokenType.DOUBLE_DOT):
            # It's a discrete range: low..high
            high = self.parse_additive()
            return RangeExpr(low=first_expr, high=high, span=self.make_span(start))

        if self.match(TokenType.RANGE):
            # Could be "Type range Low..High" or "Type range <>"
            if self.match(TokenType.BOX):
                # Unconstrained: Type range <>
                # Return SubtypeIndication with BoxConstraint
                from uada80.ast_nodes import SubtypeIndication, BoxConstraint
                return SubtypeIndication(
                    type_mark=first_expr,
                    constraint=BoxConstraint(type_mark=first_expr),
                    span=self.make_span(start)
                )
            else:
                # Constrained: Type range Low..High
                low = self.parse_additive()
                self.expect(TokenType.DOUBLE_DOT)
                high = self.parse_additive()
                from uada80.ast_nodes import SubtypeIndication, RangeConstraint
                return SubtypeIndication(
                    type_mark=first_expr,
                    constraint=RangeConstraint(
                        range_expr=RangeExpr(low=low, high=high, span=self.make_span(start))
                    ),
                    span=self.make_span(start)
                )

        # It's a subtype indication or type mark
        return first_expr

    def parse_block_statement(self, label: Optional[str] = None) -> BlockStmt:
        """Parse block statement.

        Block statements can be:
            declare ... begin ... end;
            begin ... end;
            Label: declare ... begin ... end Label;
            Label: begin ... end Label;
        """
        start = self.current
        declarations = []

        # Optional DECLARE part
        if self.match(TokenType.DECLARE):
            declarations = self.parse_declarative_part()

        self.expect(TokenType.BEGIN)
        statements = self.parse_statement_sequence()

        handlers = []
        if self.match(TokenType.EXCEPTION):
            handlers = self.parse_exception_handlers()

        self.expect(TokenType.END)

        # Optional trailing block name (must match leading label)
        trailing_label = None
        if self.check(TokenType.IDENTIFIER):
            trailing_label = self.expect_identifier()
            if label and trailing_label.lower() != label.lower():
                print(f"Parse warning: Block end label '{trailing_label}' does not match start label '{label}'")

        self.expect(TokenType.SEMICOLON)

        return BlockStmt(
            declarations=declarations, statements=statements, handled_exception_handlers=handlers,
            label=label or trailing_label, span=self.make_span(start)
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

    def parse_return_statement(self) -> ReturnStmt | ExtendedReturnStmt:
        """Parse return statement (simple or extended)."""
        start = self.current

        # Check for extended return: return Name : Type [do ... end return]
        if self.check(TokenType.IDENTIFIER) and self.peek(1).type == TokenType.COLON:
            return self.parse_extended_return_statement(start)

        # Simple return
        value = None
        if not self.check(TokenType.SEMICOLON):
            value = self.parse_expression()

        self.expect(TokenType.SEMICOLON)

        return ReturnStmt(value=value, span=self.make_span(start))

    def parse_extended_return_statement(self, start: Token) -> ExtendedReturnStmt:
        """Parse extended return statement (Ada 2005).

        Syntax: return Object_Name : [aliased] [constant] Type [:= Init] [do Stmts; end return];
        """
        object_name = self.expect_identifier()
        self.expect(TokenType.COLON)

        # Optional aliased and/or constant
        self.match(TokenType.ALIASED)
        self.match(TokenType.CONSTANT)

        # Type mark (may include constraints like Natural range 1..10)
        type_mark = self.parse_subtype_indication()

        # Optional initialization
        init_expr = None
        if self.match(TokenType.ASSIGN):
            init_expr = self.parse_expression()

        # Optional do block
        statements: list[Stmt] = []
        if self.match(TokenType.DO):
            statements = self.parse_statement_sequence()
            self.expect(TokenType.END)
            self.expect(TokenType.RETURN)

        self.expect(TokenType.SEMICOLON)

        return ExtendedReturnStmt(
            object_name=object_name,
            type_mark=type_mark,
            init_expr=init_expr,
            statements=statements,
            span=self.make_span(start),
        )

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
        """Parse select statement.

        Handles:
        - Selective accept: SELECT [WHEN guard =>] ACCEPT ... OR ... END SELECT;
        - Timed entry call: SELECT entry_call OR delay_alternative END SELECT;
        - Conditional entry call: SELECT entry_call ELSE stmts END SELECT;
        - Asynchronous select: SELECT triggering THEN ABORT abortable END SELECT;
        """
        start = self.current
        alternatives = []
        else_stmts = None
        then_abort_stmts = None

        # Parse first alternative (and subsequent OR alternatives)
        first = True
        while first or self.match(TokenType.OR):
            first = False
            guard = None

            # Check for optional guard: WHEN condition =>
            if self.match(TokenType.WHEN):
                guard = self.parse_expression()
                self.expect(TokenType.ARROW)

            # Parse the alternative content
            if self.match(TokenType.ACCEPT):
                # Accept alternative
                accept_stmt = self.parse_accept_statement()
                # After the accept, there may be additional statements before OR/END
                additional_stmts = self.parse_statement_sequence()
                stmts = [accept_stmt] + additional_stmts
                alternatives.append(SelectAlternative(guard=guard, statements=stmts))
            elif self.match(TokenType.DELAY):
                # Delay alternative
                delay_stmt = self.parse_delay_statement()
                additional_stmts = self.parse_statement_sequence()
                stmts = [delay_stmt] + additional_stmts
                alternatives.append(SelectAlternative(guard=guard, statements=stmts))
            elif self.match(TokenType.TERMINATE):
                # Terminate alternative
                self.expect(TokenType.SEMICOLON)
                alternatives.append(SelectAlternative(guard=guard, statements=[], is_terminate=True))
            else:
                # Entry call or triggering statement (for conditional/timed/async select)
                stmts = self.parse_statement_sequence()
                alternatives.append(SelectAlternative(guard=guard, statements=stmts))

        # Check for ELSE (conditional entry call)
        if self.match(TokenType.ELSE):
            else_stmts = self.parse_statement_sequence()

        # Check for THEN ABORT (asynchronous select)
        if self.match(TokenType.THEN):
            self.expect(TokenType.ABORT)
            then_abort_stmts = self.parse_statement_sequence()

        self.expect(TokenType.END)
        self.expect(TokenType.SELECT)
        self.expect(TokenType.SEMICOLON)

        return SelectStmt(
            alternatives=alternatives,
            else_statements=else_stmts,
            then_abort_statements=then_abort_stmts,
            span=self.make_span(start)
        )

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
            TokenType.AND,  # For parallel blocks: parallel do ... and do ...
            TokenType.EOF,
        ):
            stmt = self.parse_statement()
            statements.append(stmt)

        return statements

    def parse_exception_handlers(self) -> list[ExceptionHandler]:
        """Parse exception handlers.

        Syntax:
            when [occurrence_name :] exception_choice {| exception_choice} =>
                statements

        Where exception_choice can be an exception name or 'others'.
        """
        handlers = []

        while self.match(TokenType.WHEN):
            exception_names: list[Expr] = []
            occurrence_name: Optional[str] = None

            # Check for occurrence name: identifier : (before exceptions)
            if (self.check(TokenType.IDENTIFIER) and
                self.peek(1).type == TokenType.COLON and
                self.peek(2).type != TokenType.ASSIGN):  # Not a decl
                occurrence_name = self.advance().value
                self.advance()  # consume ':'

            if self.match(TokenType.OTHERS):
                exception_names = []
            else:
                exception_names.append(self.parse_name())
                while self.match(TokenType.PIPE):
                    if self.match(TokenType.OTHERS):
                        pass  # others can appear in choice list
                    else:
                        exception_names.append(self.parse_name())

            self.expect(TokenType.ARROW)
            stmts = self.parse_statement_sequence()

            handlers.append(ExceptionHandler(
                exception_names=exception_names,
                statements=stmts,
                occurrence_name=occurrence_name
            ))

        return handlers

    # ========================================================================
    # Declarations
    # ========================================================================

    def parse_declarative_part(self) -> list[Decl]:
        """Parse declarative part (sequence of declarations)."""
        declarations = []

        while not self.check(TokenType.BEGIN, TokenType.END, TokenType.PRIVATE, TokenType.EOF):
            start_pos = self.pos
            try:
                decl = self.parse_declaration()
                if decl:
                    declarations.append(decl)
                elif self.pos == start_pos:
                    # No progress made - skip this token to avoid infinite loop
                    self.advance()
            except ParseError as e:
                print(f"Parse error in declaration: {e}")
                self.synchronize()
                # Make sure we made progress
                if self.pos == start_pos:
                    self.advance()

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

        # Subprogram declaration or body (may start with overriding/not overriding)
        if self.check(TokenType.PROCEDURE, TokenType.FUNCTION, TokenType.OVERRIDING):
            return self.parse_subprogram()
        # Also check for "not overriding"
        if self.check(TokenType.NOT) and self.peek(1).type == TokenType.OVERRIDING:
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

        # Representation clause (for ... use ...)
        if self.check(TokenType.FOR):
            return self.parse_representation_clause()

        return None

    def parse_representation_clause(self) -> Optional[RepresentationClause]:
        """Parse a representation clause.

        Syntax:
            for name use ...;
            for name'attribute use ...;
            for name use record ... end record;
        """
        start = self.current
        self.expect(TokenType.FOR)

        # Parse the name (could be Type or Type'Attribute)
        name = self.parse_name()

        # Check if it's an attribute definition clause (Type'Size use ...)
        if isinstance(name, AttributeReference):
            self.expect(TokenType.USE)
            value = self.parse_expression()
            self.expect(TokenType.SEMICOLON)
            return AttributeDefinitionClause(
                name=name.prefix,
                attribute=name.attribute,
                value=value,
                span=self.make_span(start)
            )

        self.expect(TokenType.USE)

        # Check for record representation clause
        if self.match(TokenType.RECORD):
            component_clauses = []
            while not self.check(TokenType.END, TokenType.EOF):
                start_pos = self.pos
                comp = self.parse_component_clause()
                if comp:
                    component_clauses.append(comp)
                elif self.pos == start_pos:
                    # No progress - skip token to avoid infinite loop
                    self.advance()
            self.expect(TokenType.END)
            self.expect(TokenType.RECORD)
            self.expect(TokenType.SEMICOLON)
            return RecordRepresentationClause(
                type_name=name,
                component_clauses=component_clauses,
                span=self.make_span(start)
            )

        # Check for enumeration representation clause (parenthesized list)
        if self.match(TokenType.LEFT_PAREN):
            values = []
            while True:
                lit_name = self.expect_identifier()
                self.expect(TokenType.ARROW)
                lit_value = self.parse_expression()
                values.append((lit_name, lit_value))
                if not self.match(TokenType.COMMA):
                    break
            self.expect(TokenType.RIGHT_PAREN)
            self.expect(TokenType.SEMICOLON)
            return EnumerationRepresentationClause(
                type_name=name,
                values=values,
                span=self.make_span(start)
            )

        # Otherwise it's a simple value clause (rare)
        value = self.parse_expression()
        self.expect(TokenType.SEMICOLON)
        return AttributeDefinitionClause(
            name=name,
            attribute="",  # No specific attribute
            value=value,
            span=self.make_span(start)
        )

    def parse_component_clause(self) -> Optional[ComponentClause]:
        """Parse a component clause in a record representation clause.

        Syntax: name at position range first_bit .. last_bit;
        """
        start = self.current

        if self.check(TokenType.END):
            return None

        name = self.expect_identifier()
        self.expect(TokenType.AT)
        position = self.parse_expression()
        self.expect(TokenType.RANGE)
        first_bit = self.parse_expression()
        self.expect(TokenType.DOUBLE_DOT)
        last_bit = self.parse_expression()
        self.expect(TokenType.SEMICOLON)

        return ComponentClause(
            name=name,
            position=position,
            first_bit=first_bit,
            last_bit=last_bit,
            span=self.make_span(start)
        )

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

            # Parse type definition (pass is_limited for interface types)
            type_def = self.parse_type_definition(is_limited=is_limited)
        else:
            # Incomplete type declaration
            type_def = None

        # Parse optional aspect specification (Ada 2012)
        aspects = self.parse_aspect_specification()

        self.expect(TokenType.SEMICOLON)

        return TypeDecl(
            name=name,
            type_def=type_def,
            discriminants=discriminants,
            is_abstract=is_abstract,
            is_tagged=is_tagged,
            is_limited=is_limited,
            aspects=aspects,
            span=self.make_span(start),
        )

    def parse_type_definition(self, is_limited: bool = False) -> TypeDef:
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

        # Fixed point type (type T is delta 0.01 range 0.0 .. 100.0)
        if self.match(TokenType.DELTA):
            delta_expr = self.parse_expression()

            # Check for optional digits (decimal fixed point)
            digits_expr = None
            if self.match(TokenType.DIGITS):
                digits_expr = self.parse_expression()

            # Check for optional range
            range_constraint = None
            if self.match(TokenType.RANGE):
                low = self.parse_expression()
                self.expect(TokenType.DOUBLE_DOT)
                high = self.parse_expression()
                range_constraint = RangeExpr(low=low, high=high)

            return RealTypeDef(
                is_floating=False,
                delta_expr=delta_expr,
                digits_expr=digits_expr,
                range_constraint=range_constraint,
            )

        # Floating point type (type T is digits 6 range 0.0 .. 1.0)
        if self.match(TokenType.DIGITS):
            digits_expr = self.parse_expression()

            # Check for optional range
            range_constraint = None
            if self.match(TokenType.RANGE):
                low = self.parse_expression()
                self.expect(TokenType.DOUBLE_DOT)
                high = self.parse_expression()
                range_constraint = RangeExpr(low=low, high=high)

            return RealTypeDef(
                is_floating=True,
                digits_expr=digits_expr,
                range_constraint=range_constraint,
            )

        # Enumeration type
        if self.match(TokenType.LEFT_PAREN):
            literals = []
            literals.append(self._parse_enumeration_literal())
            while self.match(TokenType.COMMA):
                literals.append(self._parse_enumeration_literal())
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

            # Component type may be prefixed with "aliased"
            is_aliased = self.match(TokenType.ALIASED)
            component_type = self.parse_name()
            # Check for range constraint: array (...) of Integer range Low..High
            constraint = None
            if self.match(TokenType.RANGE):
                low = self.parse_additive()
                self.expect(TokenType.DOUBLE_DOT)
                high = self.parse_additive()
                constraint = RangeExpr(low=low, high=high, span=None)
                # Wrap in SubtypeIndication
                component_type = SubtypeIndication(
                    span=None,
                    type_mark=component_type,
                    constraint=constraint,
                )

            return ArrayTypeDef(
                index_subtypes=index_subtypes,
                component_type=component_type,
                is_aliased=is_aliased,
            )

        # Record type
        if self.match(TokenType.RECORD):
            components = []
            variant_part = None

            while not self.check(TokenType.END, TokenType.EOF):
                start_pos = self.pos
                # Handle null; for empty record (no components)
                if self.match(TokenType.NULL):
                    self.expect(TokenType.SEMICOLON)
                    break  # null; means no more components
                elif self.match(TokenType.CASE):
                    variant_part = self.parse_variant_part()
                    break
                else:
                    comp = self.parse_component_declaration()
                    if comp:
                        components.append(comp)
                    elif self.pos == start_pos:
                        # No progress - skip to avoid infinite loop
                        self.advance()

            self.expect(TokenType.END)
            self.expect(TokenType.RECORD)

            return RecordTypeDef(components=components, variant_part=variant_part)

        # Access type (may have "not null" prefix)
        is_not_null = False
        if self.check(TokenType.NOT) and self.peek(1).type == TokenType.NULL:
            self.advance()  # not
            self.advance()  # null
            is_not_null = True

        if self.match(TokenType.ACCESS):
            # Check for access-to-subprogram type
            is_protected = self.match(TokenType.PROTECTED)
            if self.check(TokenType.FUNCTION) or self.check(TokenType.PROCEDURE):
                is_function = self.match(TokenType.FUNCTION)
                if not is_function:
                    self.advance()  # consume PROCEDURE

                # Parse parameter list (optional)
                parameters = []
                if self.match(TokenType.LEFT_PAREN):
                    parameters = self.parse_formal_parameters()
                    self.expect(TokenType.RIGHT_PAREN)

                # Parse return type for functions
                return_type = None
                if is_function:
                    self.expect(TokenType.RETURN)
                    return_type = self._parse_return_type()

                return AccessSubprogramTypeDef(
                    is_function=is_function,
                    parameters=parameters,
                    return_type=return_type,
                    is_not_null=is_not_null,
                    is_access_protected=is_protected,
                )

            is_all = self.match(TokenType.ALL)
            is_constant = self.match(TokenType.CONSTANT)
            designated_type = self.parse_name()
            # Check for range constraint: access Type range Low .. High
            constraint = None
            if self.match(TokenType.RANGE):
                low = self.parse_additive()
                self.expect(TokenType.DOUBLE_DOT)
                high = self.parse_additive()
                constraint = RangeExpr(low=low, high=high, span=None)
            return AccessTypeDef(is_access_all=is_all, is_access_constant=is_constant, designated_type=designated_type, is_not_null=is_not_null, constraint=constraint)

        # Derived type
        if self.match(TokenType.NEW):
            parent_type = self.parse_name()
            record_extension = None
            interfaces: list[Expr] = []
            constraint = None
            digits_constraint = None
            delta_constraint = None

            # Check for DIGITS constraint (derived floating-point type)
            # e.g., type T is new Float_Type DIGITS 4 RANGE low .. high;
            if self.match(TokenType.DIGITS):
                digits_constraint = self.parse_expression()

            # Check for DELTA constraint (derived fixed-point type)
            # e.g., type T is new Fixed_Type DELTA 0.01 RANGE low .. high;
            if self.match(TokenType.DELTA):
                delta_constraint = self.parse_expression()

            # Check for range constraint: range Low .. High
            if self.match(TokenType.RANGE):
                low = self.parse_expression()
                self.expect(TokenType.DOUBLE_DOT)
                high = self.parse_expression()
                constraint = RangeExpr(low=low, high=high)

            # Parse implemented interfaces: and Interface1 and Interface2
            while self.match(TokenType.AND):
                interfaces.append(self.parse_name())

            # Check for record extension: with record ... end record
            # Note: Don't consume WITH here if it's for aspects (WITH followed by identifier)
            if self.check(TokenType.WITH) and self.peek(1).type == TokenType.RECORD:
                self.advance()  # consume WITH
                self.advance()  # consume RECORD
                components = []
                while not self.check(TokenType.END, TokenType.EOF):
                    start_pos = self.pos
                    comp = self.parse_component_declaration()
                    if comp:
                        components.append(comp)
                    elif self.pos == start_pos:
                        # No progress - skip to avoid infinite loop
                        self.advance()
                self.expect(TokenType.END)
                self.expect(TokenType.RECORD)
                record_extension = RecordTypeDef(components=components)

            return DerivedTypeDef(parent_type=parent_type, record_extension=record_extension, interfaces=interfaces, constraint=constraint, digits_constraint=digits_constraint, delta_constraint=delta_constraint)

        # Private type
        if self.match(TokenType.PRIVATE):
            return PrivateTypeDef()

        # Interface type (may have modifiers: task, protected, synchronized)
        # Note: limited is parsed at type declaration level and passed to us
        is_task_interface = False
        is_protected_interface = False
        is_synchronized_interface = False

        # Check for interface modifiers (limited already consumed at type decl level)
        if self.check(TokenType.TASK, TokenType.PROTECTED, TokenType.SYNCHRONIZED):
            if self.match(TokenType.TASK):
                is_task_interface = True
            if self.match(TokenType.PROTECTED):
                is_protected_interface = True
            if self.match(TokenType.SYNCHRONIZED):
                is_synchronized_interface = True

        if self.match(TokenType.INTERFACE):
            # Parse parent interfaces: and Interface1 and Interface2 ...
            parent_interfaces: list[Expr] = []
            while self.match(TokenType.AND):
                parent_interfaces.append(self.parse_name())

            return InterfaceTypeDef(
                is_limited=is_limited,  # Use is_limited from parameter
                is_task=is_task_interface,
                is_protected=is_protected_interface,
                is_synchronized=is_synchronized_interface,
                parent_interfaces=parent_interfaces,
            )

        raise ParseError("Expected type definition", self.current)

    def parse_component_declaration(self) -> ComponentDecl:
        """Parse record component declaration."""
        start = self.current
        names = [self.expect_identifier()]

        while self.match(TokenType.COMMA):
            names.append(self.expect_identifier())

        self.expect(TokenType.COLON)

        # Parse subtype indication (may include range/index constraints)
        # e.g., Natural range 1..9999 or String(1..10)
        type_mark = self.parse_subtype_indication()

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
            # Handle null; for empty variant parts
            if self.match(TokenType.NULL):
                self.expect(TokenType.SEMICOLON)
            else:
                while not self.check(TokenType.WHEN, TokenType.END, TokenType.EOF):
                    start_pos = self.pos
                    comp = self.parse_component_declaration()
                    if comp:
                        components.append(comp)
                    elif self.pos == start_pos:
                        # No progress - skip to avoid infinite loop
                        self.advance()

            variants.append(Variant(choices=choices, components=components))

        self.expect(TokenType.END)
        self.expect(TokenType.CASE)
        self.expect(TokenType.SEMICOLON)

        return VariantPart(discriminant=discriminant, variants=variants)

    def parse_discriminant_specifications(self) -> list[DiscriminantSpec]:
        """Parse discriminant specifications (semicolon-separated)."""
        specs = []

        while True:
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

            if not self.match(TokenType.SEMICOLON):
                break

        return specs

    def parse_subtype_declaration(self) -> SubtypeDecl:
        """Parse subtype declaration."""
        start = self.current
        name = self.expect_identifier()
        self.expect(TokenType.IS)
        subtype_indication = self.parse_subtype_indication()

        # Parse Ada 2012 aspects (with Static_Predicate, Dynamic_Predicate, etc.)
        aspects = self.parse_aspect_specification()

        self.expect(TokenType.SEMICOLON)

        return SubtypeDecl(
            name=name,
            subtype_indication=subtype_indication,
            aspects=aspects,
            span=self.make_span(start),
        )

    def parse_subtype_indication(self) -> SubtypeIndication:
        """Parse subtype indication.

        Handles:
        - Simple type mark: Integer
        - Range constraint: Natural range 1..100
        - Digits constraint: Float_Type digits 5
        - Delta constraint: Fixed_Type delta 0.01 range 0.0..1.0
        - Index constraint: String(1..10) - parsed as part of name
        - Discriminant constraint: Record_Type(Field => Value) - parsed as part of name
        """
        start = self.current
        type_mark = self.parse_name()
        constraint = None

        # Parse digits constraint (floating-point subtype)
        if self.match(TokenType.DIGITS):
            digits_expr = self.parse_expression()
            # Optional range after digits
            range_constraint = None
            if self.match(TokenType.RANGE):
                low = self.parse_expression()
                self.expect(TokenType.DOUBLE_DOT)
                high = self.parse_expression()
                range_constraint = RangeExpr(low=low, high=high, span=self.make_span(start))
            constraint = DigitsConstraint(digits=digits_expr, range_constraint=range_constraint)
            return SubtypeIndication(type_mark=type_mark, constraint=constraint, span=self.make_span(start))

        # Parse delta constraint (fixed-point subtype)
        if self.match(TokenType.DELTA):
            delta_expr = self.parse_expression()
            # Optional range after delta
            range_constraint = None
            if self.match(TokenType.RANGE):
                low = self.parse_expression()
                self.expect(TokenType.DOUBLE_DOT)
                high = self.parse_expression()
                range_constraint = RangeExpr(low=low, high=high, span=self.make_span(start))
            constraint = DeltaConstraint(delta=delta_expr, range_constraint=range_constraint)
            return SubtypeIndication(type_mark=type_mark, constraint=constraint, span=self.make_span(start))

        # Parse range constraint
        if self.match(TokenType.RANGE):
            low = self.parse_expression()
            self.expect(TokenType.DOUBLE_DOT)
            high = self.parse_expression()
            constraint = RangeConstraint(range_expr=RangeExpr(low=low, high=high, span=self.make_span(start)))

        # Always return SubtypeIndication for consistent interface
        return SubtypeIndication(type_mark=type_mark, constraint=constraint, span=self.make_span(start))

    def parse_object_declaration(self) -> ObjectDecl | NumberDecl | ExceptionDecl:
        """Parse object (variable/constant) declaration, number declaration, or exception declaration.

        Handles:
            X : Integer := 0;           -- object declaration
            X : Integer renames Y;      -- renaming
            Pi : constant := 3.14159;   -- number declaration (no type)
            My_Error : exception;       -- exception declaration
        """
        start = self.current
        names = [self.expect_identifier()]

        while self.match(TokenType.COMMA):
            names.append(self.expect_identifier())

        self.expect(TokenType.COLON)

        # Exception declaration: Name : exception;
        if self.match(TokenType.EXCEPTION):
            self.expect(TokenType.SEMICOLON)
            return ExceptionDecl(names=names, span=self.make_span(start))

        is_constant = self.match(TokenType.CONSTANT)
        is_aliased = self.match(TokenType.ALIASED)

        type_mark = None
        if not self.check(TokenType.ASSIGN) and not self.check(TokenType.RENAMES):
            # Check for anonymous array type: array (Index) of Element
            if self.check(TokenType.ARRAY):
                type_mark = self.parse_type_definition()
            else:
                type_mark = self.parse_subtype_indication()

        # Check for renaming declaration
        renames_expr = None
        if self.match(TokenType.RENAMES):
            renames_expr = self.parse_name()
            self.expect(TokenType.SEMICOLON)
            return ObjectDecl(
                names=names,
                type_mark=type_mark,
                is_constant=is_constant,
                is_aliased=is_aliased,
                init_expr=None,
                renames=renames_expr,
                span=self.make_span(start),
            )

        init_expr = None
        if self.match(TokenType.ASSIGN):
            init_expr = self.parse_expression()

        # Parse Ada 2012 aspects (with Volatile, with Atomic, etc.)
        aspects = self.parse_aspect_specification()

        self.expect(TokenType.SEMICOLON)

        # Number declaration: constant without type (Pi : constant := 3.14;)
        if is_constant and type_mark is None and init_expr is not None:
            return NumberDecl(
                names=names,
                value=init_expr,
                span=self.make_span(start),
            )

        return ObjectDecl(
            names=names,
            type_mark=type_mark,
            is_constant=is_constant,
            is_aliased=is_aliased,
            init_expr=init_expr,
            aspects=aspects,
            span=self.make_span(start),
        )

    def parse_subprogram(self) -> SubprogramDecl | SubprogramBody:
        """Parse subprogram declaration or body."""
        spec = self.parse_subprogram_specification()

        # Check for renaming declaration: procedure Foo renames Bar;
        if self.match(TokenType.RENAMES):
            spec.renames = self.parse_name()
            spec.aspects = self.parse_aspect_specification()
            self.expect(TokenType.SEMICOLON)
            return spec

        # Parse Ada 2012 aspects (with Inline, with Pre => Cond, etc.)
        spec.aspects = self.parse_aspect_specification()

        # Check if it's a body or just a declaration
        if self.match(TokenType.IS):
            # Body stub: procedure/function ... is separate;
            if self.match(TokenType.SEPARATE):
                self.expect(TokenType.SEMICOLON)
                kind = "function" if spec.is_function else "procedure"
                return BodyStub(name=spec.name, kind=kind, span=spec.span)

            # Generic instantiation: procedure/function X is new Y(args);
            if self.match(TokenType.NEW):
                start = self.current
                kind = "function" if spec.is_function else "procedure"
                return self.parse_generic_instantiation(kind, spec.name, start)

            # Abstract subprogram: procedure X is abstract [with Aspect => Value];
            if self.match(TokenType.ABSTRACT):
                spec.is_abstract = True
                # Parse optional aspects after 'is abstract'
                abstract_aspects = self.parse_aspect_specification()
                if abstract_aspects:
                    spec.aspects = (spec.aspects or []) + abstract_aspects
                self.expect(TokenType.SEMICOLON)
                return spec

            # Ada 2005 null procedure: procedure P is null [with Aspect => Value];
            if self.match(TokenType.NULL):
                spec.is_null = True
                # Parse optional aspects after 'is null'
                null_aspects = self.parse_aspect_specification()
                if null_aspects:
                    spec.aspects = (spec.aspects or []) + null_aspects
                self.expect(TokenType.SEMICOLON)
                return spec

            # Ada 2012 expression function: function F(...) return T is (Expr);
            if self.check(TokenType.LEFT_PAREN):
                start = self.current
                self.advance()  # consume '('

                # Check for conditional expression: (if ...)
                if self.check(TokenType.IF):
                    expr = self.parse_conditional_expr(start)
                # Check for quantified expression: (for ...)
                elif self.check(TokenType.FOR):
                    expr = self.parse_quantified_expr(start)
                # Check for case expression: (case ...)
                elif self.check(TokenType.CASE):
                    expr = self.parse_case_expr(start)
                # Ada 2022 declare expression: (declare ... begin Expr)
                elif self.check(TokenType.DECLARE):
                    expr = self.parse_declare_expr(start)
                else:
                    # Regular expression
                    expr = self.parse_expression()
                    self.expect(TokenType.RIGHT_PAREN)

                self.expect(TokenType.SEMICOLON)

                # Create a return statement from the expression
                ret_stmt = ReturnStmt(value=expr, span=None)
                return SubprogramBody(
                    spec=spec,
                    declarations=[],
                    statements=[ret_stmt],
                    handled_exception_handlers=[],
                )

            # Parse regular body
            declarations = self.parse_declarative_part()

            self.expect(TokenType.BEGIN)
            statements = self.parse_statement_sequence()

            handlers = []
            if self.match(TokenType.EXCEPTION):
                handlers = self.parse_exception_handlers()

            self.expect(TokenType.END)
            # Optional subprogram name (can be identifier or operator string)
            if self.check(TokenType.IDENTIFIER):
                self.advance()
            elif self.check(TokenType.STRING_LITERAL):
                self.advance()  # Operator name like "+"
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

        # Function names can be identifiers or operator strings like "+"
        # Child units use dotted names like "Ada.Unchecked_Deallocation"
        if is_function and self.check(TokenType.STRING_LITERAL):
            name = self.advance().value  # Operator name as string (e.g., "+", "=")
        else:
            name = self.expect_identifier()
            # Handle child unit names (dotted names like Parent.Child)
            while self.match(TokenType.DOT):
                child = self.expect_identifier()
                name = f"{name}.{child}"

        parameters = []
        if self.match(TokenType.LEFT_PAREN):
            parameters = self.parse_parameter_specifications()
            self.expect(TokenType.RIGHT_PAREN)

        return_type = None
        if is_function:
            # For generic instantiation "function Name is new ...", no return type
            if not (self.check(TokenType.IS) and self.peek(1).type == TokenType.NEW):
                self.expect(TokenType.RETURN)
                return_type = self._parse_return_type()

        return SubprogramDecl(
            name=name,
            is_function=is_function,
            parameters=parameters,
            return_type=return_type,
            is_overriding=is_overriding,
            is_not_overriding=is_not_overriding,
            span=self.make_span(start),
        )

    def _parse_return_type(self) -> Expr:
        """Parse function return type.

        Return types can be:
        - A simple type name: Integer
        - A selected type: Ada.Text_IO.File_Type
        - An anonymous access type: access Integer, access constant Integer,
          not null access Integer
        """
        start = self.current

        # Check for "not null" prefix
        not_null = False
        if self.match(TokenType.NOT):
            self.expect(TokenType.NULL)
            not_null = True

        # Check for access type indication
        if self.match(TokenType.ACCESS):
            # Optional "constant" or "protected"
            is_constant = self.match(TokenType.CONSTANT)
            is_protected = self.match(TokenType.PROTECTED)
            # Optional "all"
            is_all = self.match(TokenType.ALL)

            # Check for access-to-subprogram type: access function/procedure
            if self.check(TokenType.FUNCTION) or self.check(TokenType.PROCEDURE):
                is_function = self.match(TokenType.FUNCTION)
                if not is_function:
                    self.advance()  # consume PROCEDURE

                # Parse parameters
                parameters = []
                if self.match(TokenType.LEFT_PAREN):
                    parameters = self.parse_parameter_specifications()
                    self.expect(TokenType.RIGHT_PAREN)

                # Parse return type for functions
                return_type = None
                if is_function:
                    self.expect(TokenType.RETURN)
                    return_type = self._parse_return_type()

                return AccessSubprogramTypeIndication(
                    is_function=is_function,
                    parameters=parameters,
                    return_type=return_type,
                    is_protected=is_protected,
                    not_null=not_null,
                    span=self.make_span(start),
                )

            # The actual type for regular access types
            subtype = self.parse_name()

            return AccessTypeIndication(
                subtype=subtype,
                is_constant=is_constant,
                not_null=not_null,
                is_all=is_all,
                is_protected=is_protected,
                span=self.make_span(start),
            )

        # Regular type name
        return self.parse_name()

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

    # Alias for access-to-subprogram type parsing
    parse_formal_parameters = parse_parameter_specifications

    def parse_package(self) -> PackageDecl | PackageBody:
        """Parse package declaration or body.

        Supports child packages with dotted names like Ada.Text_IO.
        """
        start = self.current
        self.expect(TokenType.PACKAGE)

        is_body = self.match(TokenType.BODY)
        name = self.parse_dotted_name()  # Support dotted names for child packages

        if is_body:
            return self.parse_package_body(name, start)

        # Check for package renaming: package TIO renames Ada.Text_IO;
        if self.match(TokenType.RENAMES):
            renamed = self.parse_name()
            self.expect(TokenType.SEMICOLON)
            return PackageDecl(name=name, renames=renamed, span=self.make_span(start))

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
        # Skip optional end name (can be dotted like Ada.Text_IO)
        if self.check(TokenType.IDENTIFIER):
            self.parse_dotted_name()  # Consume the end name
        self.expect(TokenType.SEMICOLON)

        return PackageDecl(
            name=name, declarations=declarations, private_declarations=private_declarations, span=self.make_span(start)
        )

    def parse_package_body(self, name: str, start: Token) -> PackageBody | BodyStub:
        """Parse package body."""
        self.expect(TokenType.IS)

        # Body stub: package body Name is separate;
        if self.match(TokenType.SEPARATE):
            self.expect(TokenType.SEMICOLON)
            return BodyStub(name=name, kind="package", span=self.make_span(start))

        declarations = self.parse_declarative_part()

        statements = []
        handlers = []
        if self.match(TokenType.BEGIN):
            statements = self.parse_statement_sequence()

            if self.match(TokenType.EXCEPTION):
                handlers = self.parse_exception_handlers()

        self.expect(TokenType.END)
        # Skip optional end name (can be dotted like Ada.Text_IO)
        if self.check(TokenType.IDENTIFIER):
            self.parse_dotted_name()  # Consume the end name
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
            name = self.parse_dotted_name()  # Support child packages like Ada.Direct_IO
            pkg = self.parse_package_specification(name, start)
            pkg.generic_formals = formals
            pkg.is_generic = True  # Mark as generic even if no formals
            return pkg
        else:
            # Generic subprogram (procedure or function)
            subprog = self.parse_subprogram()
            return GenericSubprogramUnit(
                formals=formals,
                subprogram=subprog,
                span=self.make_span(start),
            )

    def parse_generic_formal(self) -> GenericFormal:
        """Parse generic formal parameter."""
        start = self.current

        if self.match(TokenType.TYPE):
            name = self.expect_identifier()

            # Check for discriminant part: type T (D : Integer) is ...
            # or unknown discriminant: type T (<>) is private
            discriminants = []
            has_unknown_discriminant = False
            if self.match(TokenType.LEFT_PAREN):
                if self.check(TokenType.BOX):
                    # Unknown discriminant: (<>)
                    self.advance()  # consume <>
                    has_unknown_discriminant = True
                else:
                    discriminants = self.parse_discriminant_specifications()
                self.expect(TokenType.RIGHT_PAREN)

            self.expect(TokenType.IS)

            # Parse generic type definition
            # Syntax:
            #   is [abstract] [tagged] [limited] private
            #   is [abstract] new Parent [with private]
            #   is (<>)  -- discrete type
            #   is range <> -- signed integer type
            #   is mod <> -- modular integer type
            #   is digits <> -- floating point type
            #   is delta <> [digits <>] -- fixed point type
            #   is array ... -- array type
            #   is access ... -- access type

            is_abstract = self.match(TokenType.ABSTRACT)
            is_tagged = self.match(TokenType.TAGGED)
            is_limited = self.match(TokenType.LIMITED)

            if self.match(TokenType.PRIVATE):
                self.expect(TokenType.SEMICOLON)
                return GenericTypeDecl(
                    name=name, is_tagged=is_tagged, is_abstract=is_abstract,
                    constraint="private", is_limited=is_limited
                )

            # Derived type: is [abstract] new Parent [with private]
            if self.match(TokenType.NEW):
                parent = self.parse_name()
                with_private = False
                if self.match(TokenType.WITH):
                    self.expect(TokenType.PRIVATE)
                    with_private = True
                self.expect(TokenType.SEMICOLON)
                return GenericTypeDecl(
                    name=name, is_abstract=is_abstract,
                    constraint="derived", parent_type=parent, with_private=with_private
                )

            # Discrete type: is (<>)
            if self.check(TokenType.LEFT_PAREN):
                if self.peek(1) and self.peek(1).type == TokenType.BOX:
                    self.advance()  # (
                    self.advance()  # <>
                    self.expect(TokenType.RIGHT_PAREN)
                    self.expect(TokenType.SEMICOLON)
                    return GenericTypeDecl(name=name, constraint="discrete")

            # Signed integer type: is range <>
            if self.match(TokenType.RANGE):
                self.expect(TokenType.BOX)
                self.expect(TokenType.SEMICOLON)
                return GenericTypeDecl(name=name, constraint="range")

            # Modular type: is mod <>
            if self.match(TokenType.MOD):
                self.expect(TokenType.BOX)
                self.expect(TokenType.SEMICOLON)
                return GenericTypeDecl(name=name, constraint="mod")

            # Floating point: is digits <>
            if self.match(TokenType.DIGITS):
                self.expect(TokenType.BOX)
                self.expect(TokenType.SEMICOLON)
                return GenericTypeDecl(name=name, constraint="digits")

            # Fixed point: is delta <> [digits <>]
            if self.match(TokenType.DELTA):
                self.expect(TokenType.BOX)
                has_digits = False
                if self.match(TokenType.DIGITS):
                    self.expect(TokenType.BOX)
                    has_digits = True
                self.expect(TokenType.SEMICOLON)
                return GenericTypeDecl(
                    name=name,
                    constraint="delta_digits" if has_digits else "delta"
                )

            # Otherwise, it's a full type definition (array, access, etc.)
            type_def = self.parse_type_definition(is_limited=is_limited)
            self.expect(TokenType.SEMICOLON)
            return GenericTypeDecl(name=name, definition=type_def)

        # Generic object formal: identifier[, identifier]* : [mode] type [:= default]
        if self.check(TokenType.IDENTIFIER):
            # Parse list of names (F, L : E)
            names = [self.expect_identifier()]
            while self.match(TokenType.COMMA):
                names.append(self.expect_identifier())
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
            # Return first name as the formal; store all names for multi-name support
            return GenericObjectDecl(
                name=names[0], names=names, mode=mode, type_ref=type_ref, default_value=default_value
            )

        # Generic subprogram formal: with procedure/function ...
        if self.match(TokenType.WITH):
            if self.match(TokenType.PROCEDURE):
                name = self.expect_identifier()
                params = []
                if self.match(TokenType.LEFT_PAREN):
                    params = self.parse_parameter_specifications()
                    self.expect(TokenType.RIGHT_PAREN)

                # Check for "is <>" or "is Name"
                is_box = False
                default_subprogram = None
                if self.match(TokenType.IS):
                    if self.match(TokenType.BOX):
                        is_box = True
                    else:
                        # Specific default subprogram name
                        default_subprogram = self.parse_name()

                self.expect(TokenType.SEMICOLON)
                return GenericSubprogramDecl(
                    name=name, kind="procedure", params=params,
                    is_box=is_box, default_subprogram=default_subprogram
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
                return_type = self._parse_return_type()

                # Check for "is <>" or "is Name"
                is_box = False
                default_subprogram = None
                if self.match(TokenType.IS):
                    if self.match(TokenType.BOX):
                        is_box = True
                    else:
                        # Specific default subprogram name
                        default_subprogram = self.parse_name()

                self.expect(TokenType.SEMICOLON)
                return GenericSubprogramDecl(
                    name=name, kind="function", params=params, return_type=return_type,
                    is_box=is_box, default_subprogram=default_subprogram
                )

            elif self.match(TokenType.PACKAGE):
                # Generic package formal: with package X is new Generic_Pkg(<>)
                # or: with package X is new Generic_Pkg (formal => actual, ...)
                name = self.expect_identifier()
                self.expect(TokenType.IS)
                self.expect(TokenType.NEW)
                generic_ref = self.parse_name()

                # Parse optional formal parameters
                actuals = []
                if self.match(TokenType.LEFT_PAREN):
                    if self.match(TokenType.BOX):
                        # (<>) means any instantiation is valid
                        pass
                    else:
                        # Parse actual parameter associations
                        while True:
                            if self.match(TokenType.BOX):
                                actuals.append(("", None))  # Box placeholder
                            else:
                                # Named or positional
                                param = self.parse_expression()
                                actuals.append(("", param))
                            if not self.match(TokenType.COMMA):
                                break
                    self.expect(TokenType.RIGHT_PAREN)
                self.expect(TokenType.SEMICOLON)
                return GenericPackageDecl(name=name, generic_ref=generic_ref, actuals=actuals)

            # Handle 'with type' - formal incomplete type
            elif self.match(TokenType.TYPE):
                name = self.expect_identifier()
                self.expect(TokenType.SEMICOLON)
                return GenericTypeDecl(name=name, constraint="incomplete")

        # If we get here with an identifier that wasn't handled by 'type', try generic object
        if self.check(TokenType.IDENTIFIER):
            # This is a fallback for generic object without explicit 'type' keyword
            obj_name = self.expect_identifier()
            if self.match(TokenType.COLON):
                mode = "in"
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

        raise ParseError(f"Unexpected token in generic formal: {self.current.type}", self.current)

    def parse_generic_instantiation(self, kind: str, name: str, start: Token) -> GenericInstantiation:
        """Parse generic instantiation: is new Generic_Name(actuals)."""
        # Parse just the generic unit name (may be qualified like Pkg.Generic_Unit)
        generic_name = self.parse_qualified_name()

        actual_parameters = []
        if self.match(TokenType.LEFT_PAREN):
            # Parse actual parameters
            while True:
                # Could be named: Formal => Actual or positional
                # Note: formal can be identifier or operator string like "="
                if self.check(TokenType.IDENTIFIER) or self.check(TokenType.STRING_LITERAL):
                    # Look ahead for =>
                    saved_pos = self.pos
                    saved_current = self.current
                    if self.check(TokenType.STRING_LITERAL):
                        param_name = self.current.value
                        self.advance()
                    else:
                        param_name = self.expect_identifier()
                    if self.match(TokenType.ARROW):
                        # Named parameter - value can be expression or operator string
                        if self.check(TokenType.STRING_LITERAL) and self.peek(1) and (
                            self.peek(1).type == TokenType.COMMA or
                            self.peek(1).type == TokenType.RIGHT_PAREN
                        ):
                            # It's an operator name as value (e.g., "=" => "=")
                            actual = Identifier(name=self.current.value, span=self.make_span(self.current))
                            self.advance()
                        else:
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

    def parse_task_declaration(self) -> Decl:
        """Parse task type declaration or task body.

        Syntax:
            task type Name is
                entry Entry_Name(Params);
            end Name;

            task body Name is
                declarations
            begin
                statements
            end Name;
        """
        start = self.current
        self.expect(TokenType.TASK)

        # Check for task body
        if self.match(TokenType.BODY):
            name = self.expect_identifier()
            self.expect(TokenType.IS)

            # Check for body stub: task body Name is separate;
            if self.match(TokenType.SEPARATE):
                self.expect(TokenType.SEMICOLON)
                return BodyStub(name=name, kind="task", span=self.make_span(start))

            declarations = self.parse_declarative_part()

            self.expect(TokenType.BEGIN)
            statements = self.parse_statement_sequence()

            # Handle exception handlers
            handlers = []
            if self.match(TokenType.EXCEPTION):
                handlers = self.parse_exception_handlers()

            self.expect(TokenType.END)
            if self.check(TokenType.IDENTIFIER):
                end_name = self.expect_identifier()
                if end_name.lower() != name.lower():
                    raise ParseError(f"end name '{end_name}' does not match task name '{name}'", self.current)
            self.expect(TokenType.SEMICOLON)

            return TaskBody(
                name=name,
                declarations=declarations,
                statements=statements,
                handled_exception_handlers=handlers,
                span=self.make_span(start),
            )

        # Task type or single task
        is_type = self.match(TokenType.TYPE)
        name = self.expect_identifier()

        entries = []
        declarations = []
        interfaces = []

        # Check for task spec
        if self.match(TokenType.IS):
            # Check for interface inheritance: is new Interface [and Interface...] with
            if self.match(TokenType.NEW):
                # Parse interface list
                interfaces.append(self.parse_name())
                while self.match(TokenType.AND):
                    interfaces.append(self.parse_name())
                # Expect WITH after interface list
                self.expect(TokenType.WITH)

            # Parse entries and other declarations
            while not self.check(TokenType.END, TokenType.EOF):
                if self.match(TokenType.ENTRY):
                    entry = self.parse_entry_declaration()
                    entries.append(entry)
                elif self.check(TokenType.PRAGMA):
                    # Skip pragmas in task spec
                    self.advance()
                    self.expect_identifier()
                    if self.match(TokenType.LEFT_PAREN):
                        while not self.check(TokenType.RIGHT_PAREN, TokenType.EOF):
                            self.advance()
                        self.expect(TokenType.RIGHT_PAREN)
                    self.expect(TokenType.SEMICOLON)
                else:
                    break

            self.expect(TokenType.END)
            if self.check(TokenType.IDENTIFIER):
                end_name = self.expect_identifier()
                if end_name.lower() != name.lower():
                    raise ParseError(f"end name '{end_name}' does not match task name '{name}'", self.current)

        self.expect(TokenType.SEMICOLON)

        return TaskTypeDecl(
            name=name,
            entries=entries,
            declarations=declarations,
            interfaces=interfaces,
            span=self.make_span(start),
        )

    def parse_task_body_impl(self, name: str, start: Token) -> TaskBody:
        """Parse task body implementation after TASK BODY name has been consumed.

        This helper is used by parse_subunit() when parsing a separate task body.
        Expects to start at IS.
        """
        self.expect(TokenType.IS)

        declarations = self.parse_declarative_part()

        self.expect(TokenType.BEGIN)
        statements = self.parse_statement_sequence()

        # Handle exception handlers
        handlers = []
        if self.match(TokenType.EXCEPTION):
            handlers = self.parse_exception_handlers()

        self.expect(TokenType.END)
        if self.check(TokenType.IDENTIFIER):
            end_name = self.expect_identifier()
            if end_name.lower() != name.lower():
                raise ParseError(f"end name '{end_name}' does not match task name '{name}'", self.current)
        self.expect(TokenType.SEMICOLON)

        return TaskBody(
            name=name,
            declarations=declarations,
            statements=statements,
            handled_exception_handlers=handlers,
            span=self.make_span(start),
        )

    def parse_entry_declaration(self) -> EntryDecl:
        """Parse an entry declaration.

        Syntax:
            entry Name;
            entry Name(Params);
            entry Name(Index : Range);  -- entry family with named index
            entry Name(1 .. 10);        -- entry family with discrete range
            entry Name(1 .. 10)(Params); -- entry family with parameters
        """
        start = self.current
        name = self.expect_identifier()

        parameters = []
        family_index = None

        if self.match(TokenType.LEFT_PAREN):
            # Could be parameters or an entry family index
            # Entry family index forms:
            #   (for I in 1..10)  - Ada 2012+ explicit form
            #   (1 .. 10)         - discrete range
            #   (Index_Type)      - discrete subtype mark
            # Parameters form:
            #   (Name : Type)     - identifier followed by colon
            if self.check(TokenType.FOR):
                # Entry family: entry E(for I in 1..10)
                self.advance()  # skip 'for'
                self.expect_identifier()  # index name
                self.expect(TokenType.IN)
                family_index = self.parse_discrete_range_or_name()
                self.expect(TokenType.RIGHT_PAREN)
            elif self._looks_like_entry_family_index():
                # Entry family with discrete range: entry F(1..3) or entry F(Index_Type)
                family_index = self.parse_discrete_range_or_name()
                self.expect(TokenType.RIGHT_PAREN)
                # Check for parameters after the family index
                if self.match(TokenType.LEFT_PAREN):
                    parameters = self.parse_parameter_specifications()
                    self.expect(TokenType.RIGHT_PAREN)
            else:
                parameters = self.parse_parameter_specifications()
                self.expect(TokenType.RIGHT_PAREN)

        self.expect(TokenType.SEMICOLON)

        return EntryDecl(
            name=name,
            parameters=parameters,
            family_index=family_index,
            span=self.make_span(start),
        )

    def parse_protected_declaration(self) -> Decl:
        """Parse protected type declaration or protected body.

        Syntax:
            protected type Name is
                procedure P;
                function F return T;
                entry E;
            private
                Data : Integer;
            end Name;

            protected body Name is
                procedure P is ... end P;
                entry E when Cond is ... end E;
            end Name;
        """
        start = self.current
        self.expect(TokenType.PROTECTED)

        # Check for protected body
        if self.match(TokenType.BODY):
            name = self.expect_identifier()
            self.expect(TokenType.IS)

            items = []
            while not self.check(TokenType.END, TokenType.EOF):
                if self.check(TokenType.PROCEDURE, TokenType.FUNCTION):
                    item = self.parse_subprogram()
                    items.append(item)
                elif self.match(TokenType.ENTRY):
                    # Entry body: entry E [(Params)] [when Cond] is ... end E;
                    entry_name = self.expect_identifier()
                    entry_params = []
                    # Parse optional parameters
                    if self.match(TokenType.LEFT_PAREN):
                        entry_params = self.parse_parameter_specifications()
                        self.expect(TokenType.RIGHT_PAREN)
                    # Parse optional barrier
                    barrier = None
                    if self.match(TokenType.WHEN):
                        barrier = self.parse_expression()
                    self.expect(TokenType.IS)
                    declarations = self.parse_declarative_part()
                    self.expect(TokenType.BEGIN)
                    statements = self.parse_statement_sequence()
                    self.expect(TokenType.END)
                    if self.check(TokenType.IDENTIFIER):
                        self.expect_identifier()
                    self.expect(TokenType.SEMICOLON)
                    # Create a body for this entry
                    entry_body = SubprogramBody(
                        spec=SubprogramDecl(
                            name=entry_name,
                            is_function=False,
                            parameters=entry_params,
                            span=self.make_span(start),
                        ),
                        declarations=declarations,
                        statements=statements,
                        span=self.make_span(start),
                    )
                    items.append(entry_body)
                else:
                    break

            self.expect(TokenType.END)
            if self.check(TokenType.IDENTIFIER):
                end_name = self.expect_identifier()
                if end_name.lower() != name.lower():
                    raise ParseError(f"end name '{end_name}' does not match protected name '{name}'", self.current)
            self.expect(TokenType.SEMICOLON)

            return ProtectedBody(
                name=name,
                items=items,
                span=self.make_span(start),
            )

        # Protected type or single protected object
        is_type = self.match(TokenType.TYPE)
        name = self.expect_identifier()

        items = []
        interfaces = []

        # Check for protected spec
        if self.match(TokenType.IS):
            # Check for interface inheritance: is new Interface [and Interface...] with
            if self.match(TokenType.NEW):
                # Parse interface list
                interfaces.append(self.parse_name())
                while self.match(TokenType.AND):
                    interfaces.append(self.parse_name())
                # Expect WITH after interface list
                self.expect(TokenType.WITH)

            # Parse public part
            while not self.check(TokenType.PRIVATE, TokenType.END, TokenType.EOF):
                if self.check(TokenType.PROCEDURE, TokenType.FUNCTION):
                    spec = self.parse_subprogram_specification()
                    self.expect(TokenType.SEMICOLON)
                    items.append(spec)
                elif self.match(TokenType.ENTRY):
                    entry = self.parse_entry_declaration()
                    items.append(entry)
                else:
                    break

            # Private part
            if self.match(TokenType.PRIVATE):
                while not self.check(TokenType.END, TokenType.EOF):
                    decl = self.parse_declaration()
                    if decl:
                        items.append(decl)
                    else:
                        break

            self.expect(TokenType.END)
            if self.check(TokenType.IDENTIFIER):
                end_name = self.expect_identifier()
                if end_name.lower() != name.lower():
                    raise ParseError(f"end name '{end_name}' does not match protected name '{name}'", self.current)

        self.expect(TokenType.SEMICOLON)

        return ProtectedTypeDecl(name=name, items=items, interfaces=interfaces, span=self.make_span(start))

    def parse_protected_body_impl(self, name: str, start: Token) -> ProtectedBody:
        """Parse protected body implementation after PROTECTED BODY name has been consumed.

        This helper is used by parse_subunit() when parsing a separate protected body.
        Expects to start at IS.
        """
        self.expect(TokenType.IS)

        items = []
        while not self.check(TokenType.END, TokenType.EOF):
            if self.check(TokenType.PROCEDURE, TokenType.FUNCTION):
                item = self.parse_subprogram()
                items.append(item)
            elif self.match(TokenType.ENTRY):
                # Entry body: entry E when Cond is ... end E;
                entry_name = self.expect_identifier()
                barrier = None
                if self.match(TokenType.WHEN):
                    barrier = self.parse_expression()
                self.expect(TokenType.IS)
                declarations = self.parse_declarative_part()
                self.expect(TokenType.BEGIN)
                statements = self.parse_statement_sequence()
                self.expect(TokenType.END)
                if self.check(TokenType.IDENTIFIER):
                    self.expect_identifier()
                self.expect(TokenType.SEMICOLON)
                # Create a body for this entry
                entry_body = SubprogramBody(
                    spec=SubprogramDecl(
                        name=entry_name,
                        is_function=False,
                        parameters=[],
                        span=self.make_span(start),
                    ),
                    declarations=declarations,
                    statements=statements,
                    span=self.make_span(start),
                )
                items.append(entry_body)
            else:
                break

        self.expect(TokenType.END)
        if self.check(TokenType.IDENTIFIER):
            end_name = self.expect_identifier()
            if end_name.lower() != name.lower():
                raise ParseError(f"end name '{end_name}' does not match protected name '{name}'", self.current)
        self.expect(TokenType.SEMICOLON)

        return ProtectedBody(
            name=name,
            items=items,
            span=self.make_span(start),
        )


def parse(source: str, filename: str = "<input>") -> Program:
    """Convenience function to parse Ada source code."""
    from .lexer import lex

    tokens = lex(source, filename)
    parser = Parser(tokens)
    return parser.parse()
