#include "Interpreter.h"
#include <iostream>

void Interpreter::interpret(const ast::ASTNodePtr& ast, std::ostream& out)
{
    interpretSymbols();
}

void Interpreter::interpretSymbols()
{
    std::vector<std::shared_ptr<Symbol>> globalSymbols;
    for (const auto& [scope, symbols] : m_symbolTable) {
    }
}
