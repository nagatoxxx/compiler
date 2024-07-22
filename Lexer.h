#pragma once

#include <cstdint>
#include <format>
#include <list>
#include <string>
#include <string_view>
#include <unordered_map>

#include "Token.h"

class Lexer
{
public:
    Lexer();
    Lexer(const Lexer&)            = delete;
    Lexer(Lexer&&)                 = delete;
    Lexer& operator=(Lexer&&)      = delete;
    Lexer& operator=(const Lexer&) = delete;

    virtual ~Lexer();

    std::list<Token> tokenize(std::string_view source);

private:
    // for lexer state machine
    enum class State : std::uint8_t
    {
        START,          // start state
        INTEGER_CONST,  // int number
        FLOATING_CONST, // floating point number
        STRING_CONST,   // string in double quotes
        OPEN_QUOTE,     // first quote
        CLOSE_QUOTE,    // second quote
        WORD,           // some word (id, type, name)
        SPACE,          // space symbols (' ', '\t', '\v', etc)
        COMMENT,        // text to ignore (start with #)
        PUNCTUATOR,     // comma, semicolon, etc
        ERROR           // unexpected character
    };

    // check if m_punctuators contains a given string
    inline bool isPunctuator(std::string_view s) const { return (m_punctuators.find(s) != m_punctuators.end()); }
    // check if m_keywords contains a given string
    inline bool isKeyword(std::string_view s) const { return (m_keywords.find(s) != m_keywords.end()); }
    // check if m_types contains a given string
    inline bool isType(std::string_view s) const { return (m_types.find(s) != m_types.end()); }

    // transition between states
    State move(State s, char c) noexcept;

private:
    static const std::unordered_map<std::string_view, TokenKind> m_punctuators;

    // chars that can be used in punctuators
    const std::string m_punctuatorsChars = "(){}[]!;.,*/-+=><";

    static const std::unordered_map<std::string_view, TokenKind> m_keywords;
    static const std::unordered_map<std::string_view, TokenKind> m_types;
};
