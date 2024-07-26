#pragma once

#include <stack>
#include <utility>

#include "AST.h"
#include "SymbolTable.h"

// works with AST, that was built by the parser
class SemanticAnalyzer
{
public:
    SemanticAnalyzer() : m_symbolTable()
    {
        m_symbolTable.enterScope(0);
        m_symbolTable[0]->makeGlobal();
    }
    ~SemanticAnalyzer() {}

    SemanticAnalyzer(const SemanticAnalyzer&)            = delete;
    SemanticAnalyzer(SemanticAnalyzer&&)                 = delete;
    SemanticAnalyzer& operator=(const SemanticAnalyzer&) = delete;
    SemanticAnalyzer& operator=(SemanticAnalyzer&&)      = delete;

    void analyze(ast::ASTNodePtr& node);

    SymbolTable& getSymbolTable() { return m_symbolTable; }

private:
    // build symbol table
    // check control flow constructions
    void traversalPreorder(const ast::ASTNodePtr& node);

    void resolveId(const ast::ASTNodePtr& node);

    // parses types, adds implicit casts
    void traversalPostorder(ast::ASTNodePtr& node);

    void resolveTypes(ast::ASTNodePtr& node);

    ts::Type getType(const ast::ASTNodePtr& node);

private:
    SymbolTable m_symbolTable;
};
