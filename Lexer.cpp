#include <cctype>
#include <cstdlib>
#include <format>
#include <iostream>

#include "Lexer.h"

const std::unordered_map<std::string_view, TokenKind> Lexer::m_punctuators = {
    {"(",  TokenKind::LPAREN },
    {")",  TokenKind::RPAREN },
    {"{",  TokenKind::LBRACE },
    {"}",  TokenKind::RBRACE },
    {"[",  TokenKind::LSQUARE},
    {"]",  TokenKind::RSQUARE},
    {";",  TokenKind::SEMI   },
    {".",  TokenKind::PERIOD },
    {"*",  TokenKind::STAR   },
    {"+",  TokenKind::PLUS   },
    {"-",  TokenKind::MINUS  },
    {"/",  TokenKind::SLASH  },
    {"=",  TokenKind::ASSIGN },
    {",",  TokenKind::COMMA  },
    {">",  TokenKind::GREATER},
    {"<",  TokenKind::LESS   },
    {">=", TokenKind::GE     },
    {"<=", TokenKind::LE     },
    {"==", TokenKind::EQUAL  },
    {"!=", TokenKind::NE     },
};

const std::unordered_map<std::string_view, TokenKind> Lexer::m_keywords = {
    {"if",     TokenKind::KW_IF    },
    {"else",   TokenKind::KW_ELSE  },
    {"const",  TokenKind::KW_CONST },
    {"return", TokenKind::KW_RETURN},
    {"true",   TokenKind::KW_TRUE  },
    {"false",  TokenKind::KW_FALSE },
    {"while",  TokenKind::KW_WHILE },
};

const std::unordered_map<std::string_view, TokenKind> Lexer::m_types = {
    {"int",   TokenKind::KW_INT  },
    {"float", TokenKind::KW_FLOAT},
    {"bool",  TokenKind::KW_BOOL },
    {"char",  TokenKind::KW_CHAR },
};

Lexer::Lexer() {}

/* virtual */ Lexer::~Lexer() {}

std::list<Token> Lexer::tokenize(std::string_view source)
{
    std::cout << "Lexer::tokenize() called" << std::endl;

    std::list<Token> tokens;

    std::string lexeme;

    State currentState = State::START;
    State lastState    = State::START; // state before next State::START
                                       // needed for adding token to output list

    std::string_view::const_iterator it        = source.cbegin(); // current char
    std::string_view::const_iterator lineStart = source.cbegin(); //

    std::size_t line = 1;

    while (true) {
        if (it == source.cend() && lexeme.empty()) {
            break;
        }

        lastState    = currentState;
        currentState = move(currentState, *it);

        switch (currentState) {
            case State::START:
            {
                // add previous lexeme to output list
                if (!lexeme.empty()) {
                    if (this->isKeyword(lexeme)) {
                        tokens.emplace_back(m_keywords.at(lexeme));
                    }

                    else if (this->isType(lexeme)) {
                        tokens.emplace_back(m_types.at(lexeme));
                    }

                    else if (lastState == State::PUNCTUATOR) {
                        if (this->isPunctuator(lexeme))
                            tokens.emplace_back(m_punctuators.at(lexeme));
                        else {
                            for (char c : lexeme) {
                                tokens.emplace_back(m_punctuators.at(std::string_view(&c, 1)));
                            }
                        }
                    }

                    else if (lastState == State::INTEGER_CONST) {
                        tokens.emplace_back(TokenKind::INTEGER_CONSTANT);
                        tokens.back().setData(std::atoi(lexeme.c_str()));
                    }

                    else if (lastState == State::FLOATING_CONST) {
                        tokens.emplace_back(TokenKind::FLOATING_CONSTANT);
                        tokens.back().setData(std::strtof(lexeme.c_str(), nullptr));
                    }

                    else if (lastState == State::CLOSE_QUOTE) {
                        tokens.emplace_back(TokenKind::STRING_CONSTANT);
                        tokens.back().setData(lexeme);
                    }

                    else if (lastState == State::WORD) {
                        tokens.emplace_back(TokenKind::IDENTIFIER);
                        tokens.back().setData(lexeme);
                    }

                    else {
                        throw LexicalError({line, static_cast<std::size_t>(it - lineStart) + 1},
                                           std::format("Invalid token: {}", lexeme));
                    }

                    std::size_t col = it - lineStart - lexeme.length() + 1;
                    tokens.back().setLoc({line, col});
                }

                lexeme.clear();
                break;
            }

            case State::INTEGER_CONST:
                [[fallthrough]];
            case State::FLOATING_CONST:
                [[fallthrough]];
            case State::WORD:
                [[fallthrough]];
            case State::STRING_CONST:
                [[fallthrough]];
            case State::PUNCTUATOR:
                lexeme += *it;
                [[fallthrough]];
            case State::SPACE:
                if (*it == '\n') {
                    line++;
                    lineStart = it + 1;
                }
                [[fallthrough]];
            case State::OPEN_QUOTE:
                [[fallthrough]];
            case State::CLOSE_QUOTE:
                [[fallthrough]];
            case State::COMMENT:
                it++;
                break;
            case State::ERROR:
                throw LexicalError({line, static_cast<std::size_t>(it - lineStart) + 1},
                                   std::format("invalid token: {}", *it));
        }
    }

    Location last = tokens.back().getLoc();
    tokens.emplace_back(TokenKind::EOS);
    tokens.back().setLoc({last.line, last.col + 1});

    std::cout << "Lexer::tokenize() success" << std::endl;

    return tokens;
}

// state transition function
Lexer::State Lexer::move(State s, char c) noexcept
{
    static char opened = '\0'; // for double symbols (quotes, etc)

    switch (s) {
        case State::START:
            if (std::isspace(c)) {
                return State::SPACE;
            }
            if (std::isdigit(c)) {
                return State::INTEGER_CONST;
            }
            if (std::isalpha(c) || c == '_') {
                return State::WORD;
            }
            if (m_punctuatorsChars.find(std::string_view(&c, 1)) != std::string::npos) {
                return State::PUNCTUATOR;
            }
            if (c == '#') {
                return State::COMMENT;
            }
            if (c == '\"') {
                opened = c;
                return State::OPEN_QUOTE;
            }

            return State::ERROR;

        case State::INTEGER_CONST:
            if (c == '.') {
                return State::FLOATING_CONST;
            }
            if (!std::isdigit(c)) {
                return State::START;
            }

            return State::INTEGER_CONST;

        // constants contain a decimal point
        case State::FLOATING_CONST:
            if (c == '.') {
                return State::ERROR;
            }
            if (!std::isdigit(c)) {
                return State::START;
            }

            return State::FLOATING_CONST;

        case State::OPEN_QUOTE:
            if (c == opened) {
                return State::CLOSE_QUOTE;
            }

            return State::STRING_CONST;

        // words in double quotes
        case State::STRING_CONST:
            if (c == opened) {
                return State::CLOSE_QUOTE;
            }

            return State::STRING_CONST;

        // any word (identifier, keyword, type)
        case State::WORD:
            if (!(std::isalnum(c) || c == '_')) {
                return State::START;
            }

            return State::WORD;

        // *, +, -, =, braces, etc
        case State::PUNCTUATOR:
            if (m_punctuatorsChars.find(std::string_view(&c, 1)) == std::string::npos) return State::START;

            return State::PUNCTUATOR;

        case State::CLOSE_QUOTE:
            return State::START;

        case State::SPACE:
            if (!std::isspace(c)) {
                return State::START;
            }

            return State::SPACE;

        // skip comments until \n
        case State::COMMENT:
            if (c == '\n') {
                return State::START;
            }

            return State::COMMENT;

        case State::ERROR:
            break;
    }
    return State::ERROR;
}
