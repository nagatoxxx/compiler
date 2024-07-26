#pragma once

#include <ostream>

#include "AST.h"
#include "SymbolTable.h"

class Interpreter
{
public:
    Interpreter(SymbolTable&& sm) : m_symbolTable(std::move(sm)) {}
    Interpreter(const Interpreter&)            = delete;
    Interpreter(Interpreter&&)                 = delete;
    Interpreter& operator=(const Interpreter&) = delete;
    Interpreter& operator=(Interpreter&&)      = delete;

    virtual ~Interpreter() {}

    // interprets AST to assembly and writes it to out stream
    void interpret(const ast::ASTNodePtr& ast, std::ostream& out = std::cout);

private:
    void interpretSymbols();

private:
    SymbolTable m_symbolTable;
};
