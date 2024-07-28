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
    void traversalPreorder(ast::ASTNodePtr& node);
    void resolveId(ast::ASTNodePtr& node);

    // parses types, adds implicit casts
    void traversalPostorder(ast::ASTNodePtr& node);
    void resolveTypes(ast::ASTNodePtr& node);

    ts::Type getType(const ast::ASTNodePtr& node);

    template <typename T>
    T calculate(const ast::ASTNodePtr& node)
    {
        ast::BinaryExpr binary = std::get<ast::BinaryExpr>(node->getData());
        std::string     op     = binary.getLiteral();

        if (op == "+") {
            return getValue<T>(node->getChildren().front()) + getValue<T>(node->getChildren().back());
        }
        else if (op == "-") {
            return getValue<T>(node->getChildren().front()) - getValue<T>(node->getChildren().back());
        }
        else if (op == "*") {
            return getValue<T>(node->getChildren().front()) * getValue<T>(node->getChildren().back());
        }
        else if (op == "/") {
            return getValue<T>(node->getChildren().front()) / getValue<T>(node->getChildren().back());
        }
        else if (op == ">") {
            return getValue<T>(node->getChildren().front()) > getValue<T>(node->getChildren().back());
        }
        else if (op == "<") {
            return getValue<T>(node->getChildren().front()) < getValue<T>(node->getChildren().back());
        }
        else if (op == "==") {
            return getValue<T>(node->getChildren().front()) == getValue<T>(node->getChildren().back());
        }
        else if (op == ">=") {
            return getValue<T>(node->getChildren().front()) >= getValue<T>(node->getChildren().back());
        }
        else if (op == "<=") {
            return getValue<T>(node->getChildren().front()) <= getValue<T>(node->getChildren().back());
        }
        else if (op == "!=") {
            return getValue<T>(node->getChildren().front()) != getValue<T>(node->getChildren().back());
        }

        return 0;
    }

    // evaluate compile-time expressions
    ast::ASTNodePtr evaluate(const ast::ASTNodePtr& node);

    // can be calculated in compile time
    bool compiletimeCalculated(const ast::ASTNodePtr& node);

private:
    SymbolTable m_symbolTable;
};
