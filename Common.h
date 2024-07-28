#pragma once

#include <cstdint>
#include <map>
#include <string>
#include <string_view>
#include <unordered_map>

#define TOK_TO_TYPE(x) static_cast<ts::Type>((x)-TokenKind::KW_INT)

namespace ts // type system namespace
{
enum Type
{
    int_t = 0,
    float_t,
    bool_t,
    char_t,
    unknown_t,
};

constexpr std::uint8_t TypeSize[]                = {/* int_t */ 4, /* float_t */ 4, /* bool_t */ 1, /* char_t */ 1};
constexpr std::string  TypeNames[]               = {"int", "float", "bool", "char", "unknown type"};
constexpr std::size_t  TypePrecedence[unknown_t] = {/* int_t */ 2, /* float_t */ 3, /* bool_t */ 1, /* char_t */ 1};

constexpr int NONCASTABLE = 0;
constexpr int SAFE_CAST   = 1;
constexpr int UNSAFE_CAST = 2;

// checks if t1 can be implicitly converted to t2
constexpr int isImplicitlyCastable(Type t1, Type t2)
{
    constexpr int castTable[unknown_t][unknown_t] = {
  // int_t
        {NONCASTABLE, SAFE_CAST,   SAFE_CAST,   UNSAFE_CAST},
 // float_t
        {UNSAFE_CAST, NONCASTABLE, SAFE_CAST,   NONCASTABLE},
 // bool_t
        {SAFE_CAST,   SAFE_CAST,   NONCASTABLE, NONCASTABLE},
 // char_t
        {SAFE_CAST,   SAFE_CAST,   SAFE_CAST,   NONCASTABLE}
    };

    return castTable[t1][t2];
}
} // namespace ts

template <typename T>
std::uint32_t valueToLong(T&& t)
{
    if constexpr (sizeof(T) != sizeof(std::uint32_t)) {
        return t;
    }
    else
        return std::bit_cast<std::uint32_t, T>(t);
}

// location in source code
struct Location
{
    std::size_t line = 0;
    std::size_t col  = 0;
};

class LexicalError
{
public:
    LexicalError(const Location& loc, std::string_view str) : m_msg(str), m_location(loc) {}

    const char* what() const throw() { return m_msg.c_str(); }

    const Location& getLocation() const { return m_location; }

private:
    std::string m_msg;
    Location    m_location;
};

class SyntaxError
{
public:
    SyntaxError(const Location& loc) : m_location(loc), m_msg("unexpected token") {}

    const char* what() const throw() { return m_msg.c_str(); }

    const Location& getLocation() const { return m_location; }

private:
    Location    m_location;
    std::string m_msg;
};

class SemanticError
{
public:
    SemanticError(const Location& loc, std::string_view what) : m_location(loc), m_msg(what) {}
    SemanticError(std::string_view what) : m_location({0, 0}), m_msg(what) {}

    const char* what() const { return m_msg.c_str(); }

    const Location& getLocation() const { return m_location; }

private:
    Location    m_location;
    std::string m_msg;
};

class InterpretError
{
public:
    InterpretError(std::string_view what) : m_what(what) {}
    const char* what() const { return m_what.c_str(); }

private:
    std::string m_what;
};
