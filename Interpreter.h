#pragma once

#include <ostream>

#include "AST.h"
#include "SymbolTable.h"

class Interpreter
{
public:
    Interpreter(SymbolTable&& sm, std::ostream& out = std::cout) : m_symbolTable(std::move(sm)), m_outputStream(out) {}
    Interpreter(const Interpreter&)            = delete;
    Interpreter(Interpreter&&)                 = delete;
    Interpreter& operator=(const Interpreter&) = delete;
    Interpreter& operator=(Interpreter&&)      = delete;

    virtual ~Interpreter() {}

    // interprets AST to assembly and writes it to out stream
    void interpret(const ast::ASTNodePtr& ast);

private:
    void interpretSymbols();
    void interpretText(const ast::ASTNodePtr& ast);
    void interpretNode(const ast::ASTNodePtr& node);

    std::string definedirectiveToASM(const ts::Type& type);
    std::string reservedirectiveToASM(const ts::Type& type);

    [[noreturn]] void error(std::string_view msg);

private:
    SymbolTable   m_symbolTable;
    std::ostream& m_outputStream;
};
