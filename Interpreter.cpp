#include <format>
#include <iostream>
#include <memory>
#include <ranges>

#include "Interpreter.h"

void Interpreter::interpret(const ast::ASTNodePtr& ast)
{
    interpretSymbols();
}

void Interpreter::interpretSymbols()
{
    // Symbol table entry
    using STEntry = std::pair<std::string, std::shared_ptr<Symbol>>;

    std::vector<STEntry> globalSymbols(m_symbolTable[0]->getSize());

    std::copy(m_symbolTable[0]->begin(), m_symbolTable[0]->end(), globalSymbols.begin());

    std::ranges::filter_view initializedGlobal = globalSymbols |
                                                 std::ranges::views::filter(
                                                     [](STEntry& s) -> bool
                                                     { return SYMBOL_GET_FLAG((*s.second), SYMBOL_FLAG_COMPILETIME); });

    m_outputStream << "section .data\n";
    for (const auto& e : initializedGlobal) {
        std::cout << '\t' << std::format("{} {} {}", e.first, typeToASM(e.second->type), e.second->value) << '\n';
    }
}

std::string Interpreter::typeToASM(const ts::Type& type)
{
    switch (type) {
        case ts::Type::int_t:
            return "dd";
        case ts::Type::float_t:
            return "dd";
        case ts::Type::bool_t:
            return "db";
        case ts::Type::char_t:
            return "db";
        default:
            error("cannot convert unknown type to assembly");
    }
}

void Interpreter::error(std::string_view msg)
{
    throw InterpretError(msg);
}
