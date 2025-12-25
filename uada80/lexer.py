"""
Lexer for Ada language.

Tokenizes Ada source code according to Ada 2012 RM Chapter 2 (Lexical Elements).
"""

from dataclasses import dataclass
from enum import Enum, auto
from typing import Iterator


class TokenType(Enum):
    """Ada token types."""

    # Keywords (Ada reserved words)
    ABORT = auto()
    ABS = auto()
    ABSTRACT = auto()
    ACCEPT = auto()
    ACCESS = auto()
    ALIASED = auto()
    ALL = auto()
    AND = auto()
    ARRAY = auto()
    AT = auto()
    BEGIN = auto()
    BODY = auto()
    CASE = auto()
    CONSTANT = auto()
    DECLARE = auto()
    DELAY = auto()
    DELTA = auto()
    DIGITS = auto()
    DO = auto()
    ELSE = auto()
    ELSIF = auto()
    END = auto()
    ENTRY = auto()
    EXCEPTION = auto()
    EXIT = auto()
    FOR = auto()
    FUNCTION = auto()
    GENERIC = auto()
    GOTO = auto()
    IF = auto()
    IN = auto()
    INTERFACE = auto()
    IS = auto()
    LIMITED = auto()
    LOOP = auto()
    MOD = auto()
    NEW = auto()
    NOT = auto()
    NULL = auto()
    OF = auto()
    OR = auto()
    OTHERS = auto()
    OUT = auto()
    OVERRIDING = auto()
    PACKAGE = auto()
    PARALLEL = auto()  # Ada 2022
    PRAGMA = auto()
    PRIVATE = auto()
    PROCEDURE = auto()
    PROTECTED = auto()
    RAISE = auto()
    RANGE = auto()
    RECORD = auto()
    REM = auto()
    RENAMES = auto()
    REQUEUE = auto()
    RETURN = auto()
    REVERSE = auto()
    SELECT = auto()
    SEPARATE = auto()
    SOME = auto()
    SUBTYPE = auto()
    SYNCHRONIZED = auto()
    TAGGED = auto()
    TASK = auto()
    TERMINATE = auto()
    THEN = auto()
    TYPE = auto()
    UNTIL = auto()
    USE = auto()
    WHEN = auto()
    WHILE = auto()
    WITH = auto()
    XOR = auto()

    # Literals
    INTEGER_LITERAL = auto()
    REAL_LITERAL = auto()
    CHARACTER_LITERAL = auto()
    STRING_LITERAL = auto()

    # Identifiers
    IDENTIFIER = auto()

    # Operators and Delimiters
    ARROW = auto()  # =>
    DOUBLE_DOT = auto()  # ..
    DOUBLE_STAR = auto()  # **
    ASSIGN = auto()  # :=
    NOT_EQUAL = auto()  # /=
    LESS_EQUAL = auto()  # <=
    GREATER_EQUAL = auto()  # >=
    LEFT_LABEL = auto()  # <<
    RIGHT_LABEL = auto()  # >>
    BOX = auto()  # <>

    # Single character delimiters
    AMPERSAND = auto()  # &
    APOSTROPHE = auto()  # '
    LEFT_PAREN = auto()  # (
    RIGHT_PAREN = auto()  # )
    STAR = auto()  # *
    PLUS = auto()  # +
    COMMA = auto()  # ,
    MINUS = auto()  # -
    DOT = auto()  # .
    SLASH = auto()  # /
    COLON = auto()  # :
    SEMICOLON = auto()  # ;
    LESS = auto()  # <
    EQUAL = auto()  # =
    GREATER = auto()  # >
    PIPE = auto()  # |
    AT_SIGN = auto()  # @ (Ada 2012 target name)
    LEFT_BRACKET = auto()  # [ (Ada 2022 container aggregates)
    RIGHT_BRACKET = auto()  # ] (Ada 2022 container aggregates)

    # Special
    EOF = auto()
    NEWLINE = auto()


# Ada reserved words (case-insensitive)
KEYWORDS = {
    "abort": TokenType.ABORT,
    "abs": TokenType.ABS,
    "abstract": TokenType.ABSTRACT,
    "accept": TokenType.ACCEPT,
    "access": TokenType.ACCESS,
    "aliased": TokenType.ALIASED,
    "all": TokenType.ALL,
    "and": TokenType.AND,
    "array": TokenType.ARRAY,
    "at": TokenType.AT,
    "begin": TokenType.BEGIN,
    "body": TokenType.BODY,
    "case": TokenType.CASE,
    "constant": TokenType.CONSTANT,
    "declare": TokenType.DECLARE,
    "delay": TokenType.DELAY,
    "delta": TokenType.DELTA,
    "digits": TokenType.DIGITS,
    "do": TokenType.DO,
    "else": TokenType.ELSE,
    "elsif": TokenType.ELSIF,
    "end": TokenType.END,
    "entry": TokenType.ENTRY,
    "exception": TokenType.EXCEPTION,
    "exit": TokenType.EXIT,
    "for": TokenType.FOR,
    "function": TokenType.FUNCTION,
    "generic": TokenType.GENERIC,
    "goto": TokenType.GOTO,
    "if": TokenType.IF,
    "in": TokenType.IN,
    "interface": TokenType.INTERFACE,
    "is": TokenType.IS,
    "limited": TokenType.LIMITED,
    "loop": TokenType.LOOP,
    "mod": TokenType.MOD,
    "new": TokenType.NEW,
    "not": TokenType.NOT,
    "null": TokenType.NULL,
    "of": TokenType.OF,
    "or": TokenType.OR,
    "others": TokenType.OTHERS,
    "out": TokenType.OUT,
    "overriding": TokenType.OVERRIDING,
    "package": TokenType.PACKAGE,
    "parallel": TokenType.PARALLEL,
    "pragma": TokenType.PRAGMA,
    "private": TokenType.PRIVATE,
    "procedure": TokenType.PROCEDURE,
    "protected": TokenType.PROTECTED,
    "raise": TokenType.RAISE,
    "range": TokenType.RANGE,
    "record": TokenType.RECORD,
    "rem": TokenType.REM,
    "renames": TokenType.RENAMES,
    "requeue": TokenType.REQUEUE,
    "return": TokenType.RETURN,
    "reverse": TokenType.REVERSE,
    "select": TokenType.SELECT,
    "separate": TokenType.SEPARATE,
    "some": TokenType.SOME,
    "subtype": TokenType.SUBTYPE,
    "synchronized": TokenType.SYNCHRONIZED,
    "tagged": TokenType.TAGGED,
    "task": TokenType.TASK,
    "terminate": TokenType.TERMINATE,
    "then": TokenType.THEN,
    "type": TokenType.TYPE,
    "until": TokenType.UNTIL,
    "use": TokenType.USE,
    "when": TokenType.WHEN,
    "while": TokenType.WHILE,
    "with": TokenType.WITH,
    "xor": TokenType.XOR,
}


@dataclass
class SourceLocation:
    """Location in source code."""

    filename: str
    line: int
    column: int

    def __str__(self) -> str:
        return f"{self.filename}:{self.line}:{self.column}"


@dataclass
class Token:
    """A lexical token."""

    type: TokenType
    value: str
    location: SourceLocation

    def __str__(self) -> str:
        return f"{self.type.name}({self.value!r}) at {self.location}"


class LexerError(Exception):
    """Lexical analysis error."""

    def __init__(self, message: str, location: SourceLocation) -> None:
        self.message = message
        self.location = location
        super().__init__(f"{location}: {message}")


class Lexer:
    """
    Lexical analyzer for Ada.

    Converts source text into a stream of tokens.
    """

    def __init__(self, source: str, filename: str = "<input>") -> None:
        self.source = source
        self.filename = filename
        self.pos = 0
        self.line = 1
        self.column = 1
        self.prev_token_type: TokenType | None = None  # Track previous token for tick vs char literal

    def current_location(self) -> SourceLocation:
        """Get current source location."""
        return SourceLocation(self.filename, self.line, self.column)

    def peek(self, offset: int = 0) -> str | None:
        """Peek at character at current position + offset."""
        pos = self.pos + offset
        if pos < len(self.source):
            return self.source[pos]
        return None

    def advance(self) -> str | None:
        """Consume and return current character."""
        if self.pos >= len(self.source):
            return None

        ch = self.source[self.pos]
        self.pos += 1

        if ch == "\n":
            self.line += 1
            self.column = 1
        else:
            self.column += 1

        return ch

    def skip_whitespace(self) -> None:
        """Skip whitespace characters including Ada format effectors."""
        # Ada format effectors: HT (09), LF (0A), VT (0B), FF (0C), CR (0D)
        while self.peek() in (" ", "\t", "\r", "\n", "\v", "\f"):
            self.advance()

    def skip_comment(self) -> None:
        """Skip Ada comment (-- to end of line)."""
        if self.peek() == "-" and self.peek(1) == "-":
            # Skip until newline
            while self.peek() and self.peek() != "\n":
                self.advance()
            # Skip the newline too
            if self.peek() == "\n":
                self.advance()

    def read_identifier(self) -> str:
        """Read an identifier or keyword.

        Ada 2012 RM 2.3 - Identifiers must:
        - Start with a letter
        - Contain only letters, digits, and underscores
        - Not have consecutive underscores
        - Not end with an underscore
        """
        start_loc = self.current_location()
        start = self.pos
        prev_was_underscore = False

        # First character must be letter (already verified by caller)
        self.advance()

        # Subsequent characters can be letters, digits, or underscores
        while self.peek() and (self.peek().isalnum() or self.peek() == "_"):
            if self.peek() == "_":
                if prev_was_underscore:
                    raise LexerError(
                        "Identifier cannot have consecutive underscores",
                        self.current_location(),
                    )
                prev_was_underscore = True
            else:
                prev_was_underscore = False
            self.advance()

        identifier = self.source[start : self.pos]

        # Check for trailing underscore
        if identifier.endswith("_"):
            raise LexerError(
                f"Identifier '{identifier}' cannot end with underscore",
                start_loc,
            )

        return identifier

    def read_number(self) -> tuple[TokenType, str]:
        """Read a numeric literal (integer or real)."""
        start = self.pos
        has_dot = False
        has_exp = False
        base = 10

        # Check for based literal (e.g., 16#FF# or Ada 83 style 16:FF:)
        if self.peek() and self.peek().isdigit():
            # Read digits
            while self.peek() and (self.peek().isdigit() or self.peek() == "_"):
                self.advance()

            # Check for # or : (based literal delimiter)
            if self.peek() == "#" or self.peek() == ":":
                delimiter = self.peek()
                base_str = self.source[start : self.pos]
                base = int(base_str.replace("_", ""))
                self.advance()  # Skip delimiter

                # Read based digits
                while self.peek() and (
                    self.peek().isalnum() or self.peek() == "_" or self.peek() == "."
                ):
                    if self.peek() == ".":
                        has_dot = True
                    self.advance()

                # Expect closing delimiter (same as opening)
                if self.peek() == delimiter:
                    self.advance()
                else:
                    raise LexerError(f"Expected '{delimiter}' to close based literal", self.current_location())

                # Check for exponent
                if self.peek() and self.peek().upper() == "E":
                    has_exp = True
                    self.advance()
                    if self.peek() and self.peek() in "+-":
                        self.advance()
                    while self.peek() and (self.peek().isdigit() or self.peek() == "_"):
                        self.advance()

            # Check for decimal point
            elif self.peek() == "." and self.peek(1) and self.peek(1).isdigit():
                has_dot = True
                self.advance()  # Skip .
                while self.peek() and (self.peek().isdigit() or self.peek() == "_"):
                    self.advance()

            # Check for exponent (decimal only)
            if not has_exp and self.peek() and self.peek().upper() == "E":
                has_exp = True
                self.advance()
                if self.peek() and self.peek() in "+-":
                    self.advance()
                while self.peek() and (self.peek().isdigit() or self.peek() == "_"):
                    self.advance()

        value = self.source[start : self.pos]

        # Per Ada 2012 RM 2.4: Only presence of decimal point makes it real.
        # Exponent alone (e.g., 1E6) is still an integer literal.
        if has_dot:
            return (TokenType.REAL_LITERAL, value)
        else:
            return (TokenType.INTEGER_LITERAL, value)

    def read_string(self) -> str:
        """Read a string literal (supports both " and obsolescent % delimiter)."""
        start_loc = self.current_location()
        delimiter = self.advance()  # Skip opening delimiter (" or %)
        chars = []

        while True:
            ch = self.peek()

            if ch is None:
                raise LexerError("Unterminated string literal", start_loc)

            if ch == delimiter:
                self.advance()
                # Check for doubled delimiter (escaped quote in Ada)
                if self.peek() == delimiter:
                    chars.append(delimiter)
                    self.advance()
                else:
                    break
            else:
                chars.append(ch)
                self.advance()

        return "".join(chars)

    def read_character(self) -> str:
        """Read a character literal."""
        start_loc = self.current_location()
        self.advance()  # Skip opening '

        ch = self.peek()
        if ch is None:
            raise LexerError("Unterminated character literal", start_loc)

        self.advance()

        if self.peek() != "'":
            raise LexerError("Expected closing ' for character literal", self.current_location())

        self.advance()  # Skip closing '

        return ch

    def next_token(self) -> Token:
        """Get the next token from input."""
        # Skip whitespace and comments
        while True:
            self.skip_whitespace()
            if self.peek() == "-" and self.peek(1) == "-":
                self.skip_comment()
            else:
                break

        location = self.current_location()
        ch = self.peek()

        # EOF
        if ch is None:
            return Token(TokenType.EOF, "", location)

        # Identifiers and keywords
        if ch.isalpha():
            identifier = self.read_identifier()
            # Check if it's a keyword (case-insensitive)
            token_type = KEYWORDS.get(identifier.lower(), TokenType.IDENTIFIER)
            return Token(token_type, identifier, location)

        # Numbers
        if ch.isdigit():
            token_type, value = self.read_number()
            return Token(token_type, value, location)

        # String literals (including obsolescent % delimiter)
        if ch == '"' or ch == '%':
            value = self.read_string()
            return Token(TokenType.STRING_LITERAL, value, location)

        # Character literals (tricky: apostrophe is also attribute marker)
        # Character literal: 'x'  Attribute: Variable'First
        # Per Ada RM 2.2, apostrophe after these tokens is a tick (attribute marker):
        # - identifier, keyword ALL, RIGHT_PAREN, RIGHT_BRACKET
        # Otherwise, if pattern matches 'x', it's a character literal
        if ch == "'":
            # Check if previous token makes this a tick (attribute/qualified expression)
            tick_predecessors = {
                TokenType.IDENTIFIER,
                TokenType.RIGHT_PAREN,
                TokenType.RIGHT_BRACKET,
                TokenType.ALL,
                # Also keywords that can be type names followed by 'Range etc.
                TokenType.ACCESS,
                TokenType.DELTA,
                TokenType.DIGITS,
                TokenType.MOD,
                # Note: RANGE is NOT included here because after "Type RANGE 'A'"
                # we need 'A' to be a character literal, not an apostrophe.
                # When used as attribute ('Range), the lexer produces IDENTIFIER
                # for "Range", not the RANGE keyword.
            }
            if self.prev_token_type in tick_predecessors:
                # It's an apostrophe for attributes/qualified expressions
                self.advance()
                return Token(TokenType.APOSTROPHE, "'", location)

            # Otherwise, check if it looks like a character literal
            next_ch = self.peek(1)
            next_next = self.peek(2)

            # Character literal pattern: 'x' where x is any character
            if next_ch and next_next == "'":
                value = self.read_character()
                return Token(TokenType.CHARACTER_LITERAL, value, location)
            else:
                # It's an apostrophe for attributes
                self.advance()
                return Token(TokenType.APOSTROPHE, "'", location)

        # Two-character operators
        if ch == "=" and self.peek(1) == ">":
            self.advance()
            self.advance()
            return Token(TokenType.ARROW, "=>", location)

        if ch == "." and self.peek(1) == ".":
            self.advance()
            self.advance()
            return Token(TokenType.DOUBLE_DOT, "..", location)

        if ch == "*" and self.peek(1) == "*":
            self.advance()
            self.advance()
            return Token(TokenType.DOUBLE_STAR, "**", location)

        if ch == ":" and self.peek(1) == "=":
            self.advance()
            self.advance()
            return Token(TokenType.ASSIGN, ":=", location)

        if ch == "/" and self.peek(1) == "=":
            self.advance()
            self.advance()
            return Token(TokenType.NOT_EQUAL, "/=", location)

        if ch == "<" and self.peek(1) == "=":
            self.advance()
            self.advance()
            return Token(TokenType.LESS_EQUAL, "<=", location)

        if ch == ">" and self.peek(1) == "=":
            self.advance()
            self.advance()
            return Token(TokenType.GREATER_EQUAL, ">=", location)

        if ch == "<" and self.peek(1) == "<":
            self.advance()
            self.advance()
            return Token(TokenType.LEFT_LABEL, "<<", location)

        if ch == ">" and self.peek(1) == ">":
            self.advance()
            self.advance()
            return Token(TokenType.RIGHT_LABEL, ">>", location)

        if ch == "<" and self.peek(1) == ">":
            self.advance()
            self.advance()
            return Token(TokenType.BOX, "<>", location)

        # Single character tokens
        single_char_tokens = {
            "&": TokenType.AMPERSAND,
            "(": TokenType.LEFT_PAREN,
            ")": TokenType.RIGHT_PAREN,
            "*": TokenType.STAR,
            "+": TokenType.PLUS,
            ",": TokenType.COMMA,
            "-": TokenType.MINUS,
            ".": TokenType.DOT,
            "/": TokenType.SLASH,
            ":": TokenType.COLON,
            ";": TokenType.SEMICOLON,
            "<": TokenType.LESS,
            "=": TokenType.EQUAL,
            ">": TokenType.GREATER,
            "|": TokenType.PIPE,
            "!": TokenType.PIPE,  # Obsolescent alternative to |
            "@": TokenType.AT_SIGN,
            "[": TokenType.LEFT_BRACKET,
            "]": TokenType.RIGHT_BRACKET,
        }

        if ch in single_char_tokens:
            self.advance()
            return Token(single_char_tokens[ch], ch, location)

        # Unknown character
        raise LexerError(f"Unexpected character: {ch!r}", location)

    def tokenize(self) -> Iterator[Token]:
        """Generate all tokens from the source."""
        while True:
            token = self.next_token()
            yield token
            self.prev_token_type = token.type
            if token.type == TokenType.EOF:
                break


def lex(source: str, filename: str = "<input>") -> list[Token]:
    """Convenience function to tokenize source into a list of tokens."""
    lexer = Lexer(source, filename)
    return list(lexer.tokenize())
