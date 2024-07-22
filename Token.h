#pragma once

#include <any>
#include <string>

#include "Common.h"
#include "TokenKind.h"

class Token
{
public:
    Token(TokenKind k);
    virtual ~Token();

    void setData(std::any data) { m_data = data; }
    void setLoc(const Location& loc) { m_loc = loc; }

    [[nodiscard]] TokenKind          getKind() const { return m_kind; }
    [[nodiscard]] int                getInteger() const;
    [[nodiscard]] float              getFloat() const;
    [[nodiscard]] const std::string& getString() const;
    [[nodiscard]] const Location&    getLoc() const { return m_loc; }

    [[nodiscard]] bool is(TokenKind kind) const { return m_kind == kind; }

private:
    TokenKind m_kind;
    std::any  m_data;
    Location  m_loc;
};
