#include <cassert>

#include "Token.h"

Token::Token(TokenKind k) : m_kind(k) {}

/* virtual */ Token::~Token() {}

int Token::getInteger() const
{
    assert(m_data.has_value());
    assert("Token must be numeric constant" && (m_kind == TokenKind::INTEGER_CONSTANT));

    return std::any_cast<int>(m_data);
}

float Token::getFloat() const
{
    assert(m_data.has_value());
    assert("Token must be numeric constant" && (m_kind == TokenKind::FLOATING_CONSTANT));

    return std::any_cast<float>(m_data);
}

const std::string& Token::getString() const
{
    assert(m_data.has_value());
    assert("Token must be string constant" && (m_kind == TokenKind::STRING_CONSTANT || m_kind == TokenKind::IDENTIFIER));

    return std::any_cast<const std::string&>(m_data);
}
