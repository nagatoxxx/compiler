#pragma once

#include "AST.h"
#include "SymbolTable.h"

// works with AST, that was built by the parser
class SemanticAnalyzer
{
public:
    SemanticAnalyzer() : m_scopes() {}
    ~SemanticAnalyzer() {}

    SemanticAnalyzer(const SemanticAnalyzer&)            = delete;
    SemanticAnalyzer(SemanticAnalyzer&&)                 = delete;
    SemanticAnalyzer& operator=(const SemanticAnalyzer&) = delete;
    SemanticAnalyzer& operator=(SemanticAnalyzer&&)      = delete;

    void analyze(ast::ASTNodePtr& node);

private:
    // build symbol table
    // check control constructions
    void traversalPreorder(const ast::ASTNodePtr& node);

    void IdResolution(const ast::ASTNodePtr& node);
    void flowControlCheck(const ast::ASTNodePtr& node);

    // parses types, adds implicit casts
    void traversalPostorder(ast::ASTNodePtr& node);

    void typeCheck(ast::ASTNodePtr& node);

    ts::Type getType(const ast::ASTNodePtr& node);

private:
    Scopes m_scopes;

    struct State
    {
        bool ifNodeVisited = false;
    } m_state;
};
