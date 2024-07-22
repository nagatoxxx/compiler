#pragma once

#define IS_TYPENAME(x) ((x) >= TokenKind::KW_INT && (x) <= TokenKind::KW_CHAR)
#define IS_RELOP(x)    ((x) >= TokenKind::GREATER && (x) <= TokenKind::NE)

enum TokenKind
{
    // common tokens
    IDENTIFIER = 0,    // abcde123
    INTEGER_CONSTANT,  // integer constants
    FLOATING_CONSTANT, // floating point
    STRING_CONSTANT,   // string constant (in double quotes)
    LPAREN,            // "("
    RPAREN,            // ")"
    LBRACE,            // "{"
    RBRACE,            // "}"
    LSQUARE,           // "["
    RSQUARE,           // "]
    SEMI,              // ";"

    // operators
    PERIOD,  // "."
    STAR,    // "*"
    PLUS,    // "+"
    MINUS,   // "-"
    SLASH,   // "/"
    ASSIGN,  // "="
    COMMA,   // ","
    GREATER, // ">"
    LESS,    // "<"
    GE,      // ">="
    LE,      // "<="
    EQUAL,   // "=="
    NE,      // "!="

    // types
    KW_INT,
    KW_FLOAT,
    KW_BOOL,
    KW_CHAR,

    // keyword
    KW_CONST,
    KW_TRUE,
    KW_FALSE,
    KW_IF,
    KW_ELSE,
    KW_WHILE,
    KW_RETURN,

    EOS,

    NUM // amount of TokenKinds
};

static const char* const TokNames[] =
    {"IDENTIFIER",
     "INTEGER_CONSTANT",
     "FLOATING_CONSTANT",
     "STRING_CONSTANT",
     "LPAREN",
     "RPAREN",
     "LBRACE",
     "RBRACE",
     "LSQUARE",
     "RSQUARE",
     "SEMICOLON",
     "PERIOD",
     "STAR",
     "PLUS",
     "MINUS",
     "SLASH",
     "ASSIGN",
     "COMMA",
     "GREATER",
     "LESS",
     "GE",
     "LE",
     "EQUAL",
     "NE",
     "KW_INT",
     "KW_FLOAT",
     "KW_BOOL",
     "KW_CHAR",
     "KW_CONST",
     "KW_TRUE",
     "KW_FALSE",
     "KW_IF",
     "KW_ELSE",
     "KW_WHILE",
     "KW_RETURN",
     "EOS",
     nullptr};
