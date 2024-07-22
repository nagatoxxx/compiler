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
    ast::ASTNodePtr branch_stmt();
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

    // parsing body of a statement
    template <typename T>
        requires(std::is_same_v<T, ast::BodyThen> || std::is_same_v<T, ast::BodyElse>)
    ast::ASTNodePtr body()
    {
        ast::ASTNodePtr body = std::make_shared<ast::ASTNode>(T());
        if (m_ct->getKind() == TokenKind::LBRACE) {
            eat();
            body->addChild(std::make_shared<ast::ASTNode>(ast::BlockStart()));
            while (m_ct->getKind() != TokenKind::RBRACE) {
                body->addChild(statement());
            }
            body->addChild(std::make_shared<ast::ASTNode>(ast::BlockEnd()));
            eat();
        }
        else
            body->addChild(statement());

        return body;
    }

    // flow control

    // throw exception
    [[noreturn]] void error();

private:
    std::list<Token>::const_iterator m_ct; // current token
};
