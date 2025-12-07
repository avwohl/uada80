"""Tests for the Ada lexer."""

import pytest
from uada80.lexer import Lexer, TokenType, LexerError, lex


def test_keywords():
    """Test recognition of Ada keywords."""
    source = "procedure begin end if then else"
    tokens = lex(source)

    assert tokens[0].type == TokenType.PROCEDURE
    assert tokens[1].type == TokenType.BEGIN
    assert tokens[2].type == TokenType.END
    assert tokens[3].type == TokenType.IF
    assert tokens[4].type == TokenType.THEN
    assert tokens[5].type == TokenType.ELSE
    assert tokens[6].type == TokenType.EOF


def test_keywords_case_insensitive():
    """Test that keywords are case-insensitive."""
    source = "PROCEDURE Procedure procedure"
    tokens = lex(source)

    assert tokens[0].type == TokenType.PROCEDURE
    assert tokens[1].type == TokenType.PROCEDURE
    assert tokens[2].type == TokenType.PROCEDURE


def test_identifiers():
    """Test identifier recognition."""
    source = "Hello_World X Y2 My_Var"
    tokens = lex(source)

    assert tokens[0].type == TokenType.IDENTIFIER
    assert tokens[0].value == "Hello_World"
    assert tokens[1].type == TokenType.IDENTIFIER
    assert tokens[1].value == "X"
    assert tokens[2].type == TokenType.IDENTIFIER
    assert tokens[2].value == "Y2"
    assert tokens[3].type == TokenType.IDENTIFIER
    assert tokens[3].value == "My_Var"


def test_integer_literals():
    """Test integer literal recognition."""
    source = "42 1_000 16#FF# 2#1010# 8#77#"
    tokens = lex(source)

    assert tokens[0].type == TokenType.INTEGER_LITERAL
    assert tokens[0].value == "42"
    assert tokens[1].type == TokenType.INTEGER_LITERAL
    assert tokens[1].value == "1_000"
    assert tokens[2].type == TokenType.INTEGER_LITERAL
    assert tokens[2].value == "16#FF#"
    assert tokens[3].type == TokenType.INTEGER_LITERAL
    assert tokens[3].value == "2#1010#"
    assert tokens[4].type == TokenType.INTEGER_LITERAL
    assert tokens[4].value == "8#77#"


def test_real_literals():
    """Test real (floating-point) literal recognition."""
    source = "3.14159 1.0E-6 2.5E+3"
    tokens = lex(source)

    assert tokens[0].type == TokenType.REAL_LITERAL
    assert tokens[0].value == "3.14159"
    assert tokens[1].type == TokenType.REAL_LITERAL
    assert tokens[1].value == "1.0E-6"
    assert tokens[2].type == TokenType.REAL_LITERAL
    assert tokens[2].value == "2.5E+3"


def test_string_literals():
    """Test string literal recognition."""
    source = '"Hello" "World" "Quote:"" here"'
    tokens = lex(source)

    assert tokens[0].type == TokenType.STRING_LITERAL
    assert tokens[0].value == "Hello"
    assert tokens[1].type == TokenType.STRING_LITERAL
    assert tokens[1].value == "World"
    assert tokens[2].type == TokenType.STRING_LITERAL
    assert tokens[2].value == 'Quote:" here'


def test_character_literals():
    """Test character literal recognition."""
    source = "'A' 'x' '0'"
    tokens = lex(source)

    assert tokens[0].type == TokenType.CHARACTER_LITERAL
    assert tokens[0].value == "A"
    assert tokens[1].type == TokenType.CHARACTER_LITERAL
    assert tokens[1].value == "x"
    assert tokens[2].type == TokenType.CHARACTER_LITERAL
    assert tokens[2].value == "0"


def test_operators():
    """Test operator recognition."""
    source = "+ - * / ** := => /= <= >= < > = .. <>"
    tokens = lex(source)

    assert tokens[0].type == TokenType.PLUS
    assert tokens[1].type == TokenType.MINUS
    assert tokens[2].type == TokenType.STAR
    assert tokens[3].type == TokenType.SLASH
    assert tokens[4].type == TokenType.DOUBLE_STAR
    assert tokens[5].type == TokenType.ASSIGN
    assert tokens[6].type == TokenType.ARROW
    assert tokens[7].type == TokenType.NOT_EQUAL
    assert tokens[8].type == TokenType.LESS_EQUAL
    assert tokens[9].type == TokenType.GREATER_EQUAL
    assert tokens[10].type == TokenType.LESS
    assert tokens[11].type == TokenType.GREATER
    assert tokens[12].type == TokenType.EQUAL
    assert tokens[13].type == TokenType.DOUBLE_DOT
    assert tokens[14].type == TokenType.BOX


def test_delimiters():
    """Test delimiter recognition."""
    source = "( ) , ; : . | & << >>"
    tokens = lex(source)

    assert tokens[0].type == TokenType.LEFT_PAREN
    assert tokens[1].type == TokenType.RIGHT_PAREN
    assert tokens[2].type == TokenType.COMMA
    assert tokens[3].type == TokenType.SEMICOLON
    assert tokens[4].type == TokenType.COLON
    assert tokens[5].type == TokenType.DOT
    assert tokens[6].type == TokenType.PIPE
    assert tokens[7].type == TokenType.AMPERSAND
    assert tokens[8].type == TokenType.LEFT_LABEL
    assert tokens[9].type == TokenType.RIGHT_LABEL


def test_comments():
    """Test comment handling."""
    source = """
    X := 42;  -- This is a comment
    -- Full line comment
    Y := 10;
    """
    tokens = lex(source)

    # Should skip comments
    assert tokens[0].type == TokenType.IDENTIFIER  # X
    assert tokens[1].type == TokenType.ASSIGN
    assert tokens[2].type == TokenType.INTEGER_LITERAL  # 42
    assert tokens[3].type == TokenType.SEMICOLON
    assert tokens[4].type == TokenType.IDENTIFIER  # Y


def test_simple_procedure():
    """Test lexing a simple procedure."""
    source = """
    procedure Hello is
    begin
        Put_Line("Hello, World!");
    end Hello;
    """
    tokens = lex(source)

    assert tokens[0].type == TokenType.PROCEDURE
    assert tokens[1].type == TokenType.IDENTIFIER
    assert tokens[1].value == "Hello"
    assert tokens[2].type == TokenType.IS
    assert tokens[3].type == TokenType.BEGIN
    assert tokens[4].type == TokenType.IDENTIFIER
    assert tokens[4].value == "Put_Line"
    assert tokens[5].type == TokenType.LEFT_PAREN
    assert tokens[6].type == TokenType.STRING_LITERAL
    assert tokens[6].value == "Hello, World!"
    assert tokens[7].type == TokenType.RIGHT_PAREN
    assert tokens[8].type == TokenType.SEMICOLON
    assert tokens[9].type == TokenType.END
    assert tokens[10].type == TokenType.IDENTIFIER
    assert tokens[10].value == "Hello"
    assert tokens[11].type == TokenType.SEMICOLON


def test_whitespace_handling():
    """Test that whitespace is properly skipped."""
    source = "X    :=\t42 ;   Y := 10;"
    tokens = lex(source)

    assert tokens[0].type == TokenType.IDENTIFIER
    assert tokens[1].type == TokenType.ASSIGN
    assert tokens[2].type == TokenType.INTEGER_LITERAL
    assert tokens[3].type == TokenType.SEMICOLON
    assert tokens[4].type == TokenType.IDENTIFIER


def test_location_tracking():
    """Test source location tracking."""
    source = "X := 42;\nY := 10;"
    lexer = Lexer(source)
    tokens = list(lexer.tokenize())

    assert tokens[0].location.line == 1
    assert tokens[0].location.column == 1
    # Y should be on line 2
    assert tokens[4].location.line == 2


def test_unterminated_string_error():
    """Test error on unterminated string."""
    source = '"Hello'
    with pytest.raises(LexerError) as exc_info:
        lex(source)
    assert "Unterminated string" in str(exc_info.value)


def test_invalid_character_error():
    """Test error on invalid character."""
    source = "X := $"
    with pytest.raises(LexerError) as exc_info:
        lex(source)
    assert "Unexpected character" in str(exc_info.value)


def test_apostrophe_attribute():
    """Test apostrophe for attributes."""
    source = "My_Array'First"
    tokens = lex(source)

    assert tokens[0].type == TokenType.IDENTIFIER
    assert tokens[0].value == "My_Array"
    assert tokens[1].type == TokenType.APOSTROPHE
    assert tokens[2].type == TokenType.IDENTIFIER
    assert tokens[2].value == "First"


def test_at_symbol():
    """Test @ (target name) for Ada 2012."""
    source = "X := @ + 1;"
    tokens = lex(source)

    assert tokens[0].type == TokenType.IDENTIFIER
    assert tokens[1].type == TokenType.ASSIGN
    assert tokens[2].type == TokenType.AT_SIGN
    assert tokens[2].value == "@"
    assert tokens[3].type == TokenType.PLUS


def test_consecutive_underscores_error():
    """Test error on consecutive underscores in identifier."""
    source = "My__Var"
    with pytest.raises(LexerError) as exc_info:
        lex(source)
    assert "consecutive underscores" in str(exc_info.value)


def test_trailing_underscore_error():
    """Test error on trailing underscore in identifier."""
    source = "My_Var_"
    with pytest.raises(LexerError) as exc_info:
        lex(source)
    assert "cannot end with underscore" in str(exc_info.value)


def test_valid_underscored_identifier():
    """Test valid identifier with underscores."""
    source = "My_Long_Variable_Name"
    tokens = lex(source)

    assert tokens[0].type == TokenType.IDENTIFIER
    assert tokens[0].value == "My_Long_Variable_Name"


def test_integer_with_exponent():
    """Test integer literal with exponent (1E6 is integer in Ada)."""
    source = "1E6 2E+10 3E-5"
    tokens = lex(source)

    # Per Ada 2012 RM 2.4, exponent without decimal point is still integer
    assert tokens[0].type == TokenType.INTEGER_LITERAL
    assert tokens[0].value == "1E6"
    assert tokens[1].type == TokenType.INTEGER_LITERAL
    assert tokens[1].value == "2E+10"
    assert tokens[2].type == TokenType.INTEGER_LITERAL
    assert tokens[2].value == "3E-5"


def test_real_with_exponent():
    """Test real literal with exponent (decimal point makes it real)."""
    source = "1.0E6 2.5E+10"
    tokens = lex(source)

    assert tokens[0].type == TokenType.REAL_LITERAL
    assert tokens[0].value == "1.0E6"
    assert tokens[1].type == TokenType.REAL_LITERAL
    assert tokens[1].value == "2.5E+10"


def test_based_literal_with_exponent():
    """Test based literal with exponent."""
    source = "16#FF#E2"
    tokens = lex(source)

    assert tokens[0].type == TokenType.INTEGER_LITERAL
    assert tokens[0].value == "16#FF#E2"


def test_based_real_literal():
    """Test based real literal (with decimal point)."""
    source = "16#F.FF#"
    tokens = lex(source)

    assert tokens[0].type == TokenType.REAL_LITERAL
    assert tokens[0].value == "16#F.FF#"
