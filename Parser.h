#pragma once

#include <array>
#include <cstdint>
#include <format>
#include <functional>
#include <list>
#include <unordered_map>
#include <utility>
#include <variant>

#include "AST.h"
#include "SymbolTable.h"
#include "Token.h"

class Parser
{
public:
    Parser();
    Parser(const Parser& parser)            = delete;
    Parser(Parser&& parser)                 = delete;
    Parser& operator=(const Parser& parser) = delete;
    Parser& operator=(Parser&& parser)      = delete;

    virtual ~Parser();

    // checks whether a given sequence of tokens satisfies a language grammar
    ast::ASTNodePtr parse(const std::list<Token>& tokens);

    // private:
    // go to next token
    // calls error() if m_currentToken != token
    // otherwise, m_currentToken++
    void eat(const TokenKind& token);

    // m_currentToken++ without checking
    void eat();

    //==-- parsing functions --==//
    ast::ASTNodePtr program(); // main parsing function
    ast::ASTNodePtr statement();
    ast::ASTNodePtr declaration_stmt();
    ast::ASTNodePtr if_stmt();
    ast::ASTNodePtr else_stmt();
    ast::ASTNodePtr while_stmt();
    ast::ASTNodePtr assignment_stmt();

    // arithmetic expressions
    ast::ASTNodePtr expr();
    ast::ASTNodePtr relation_expr();
    ast::ASTNodePtr relation_tail(const ast::ASTNodePtr& left);
    ast::ASTNodePtr additive_expr();
    ast::ASTNodePtr additive_tail(const ast::ASTNodePtr& left);
    ast::ASTNodePtr multiplicative_expr();
    ast::ASTNodePtr multiplicative_tail(const ast::ASTNodePtr& left);
    ast::ASTNodePtr unary_expr();
    ast::ASTNodePtr access_expr();
    ast::ASTNodePtr access_tail(const ast::ASTNodePtr& left);
    ast::ASTNodePtr primary();

    ast::ASTNodePtr body();

    // flow control

    // throw exception
    [[noreturn]] void error();

private:
    std::list<Token>::const_iterator m_ct; // current token
};
