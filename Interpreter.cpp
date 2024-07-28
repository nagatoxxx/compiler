#include <format>
#include <memory>
#include <ranges>

#include "Interpreter.h"

void Interpreter::interpret(const ast::ASTNodePtr& ast)
{
    interpretSymbols();
    interpretText(ast);
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

    if (!initializedGlobal.empty()) {
        m_outputStream << "section .data\n";
    }
    for (const auto& e : initializedGlobal) {
        m_outputStream << '\t' << std::format("{} {} {}\n", e.first, definedirectiveToASM(e.second->type), e.second->value);
    }

    std::ranges::filter_view
        uninitGlobal = globalSymbols |
                       std::ranges::views::filter(
                           [](STEntry& s) -> bool {
                               return !(SYMBOL_GET_FLAG((*s.second), SYMBOL_FLAG_INITIALIZED | SYMBOL_FLAG_COMPILETIME));
                           });

    if (!uninitGlobal.empty()) {
        m_outputStream << "\nsection .bss\n";
    }
    for (const auto& e : uninitGlobal) {
        m_outputStream << '\t'
                       << std::format("{} {} {}\n",
                                      e.first,
                                      reservedirectiveToASM(e.second->type),
                                      e.second->size / ts::TypeSize[e.second->type]);
    }
}

void Interpreter::interpretText(const ast::ASTNodePtr& ast)
{
    m_outputStream << "\nsection .text\n\tglobal _start\n\n_start:\n";
}

void Interpreter::interpretNode(const ast::ASTNodePtr& node)
{
    std::visit(
        [&node, this](auto&& arg) -> void
        {
            using T = std::decay_t<decltype(arg)>;

            if constexpr (std::is_same_v<T, ast::BinaryExpr>) {
            }
        },
        node->getData());
}

std::string Interpreter::definedirectiveToASM(const ts::Type& type)
{
    switch (type) {
        case ts::Type::int_t:
            [[fallthrough]];
        case ts::Type::float_t:
            return "dd";
        case ts::Type::bool_t:
            [[fallthrough]];
        case ts::Type::char_t:
            return "db";
        default:
            error("cannot convert unknown type to assembly");
    }
}

std::string Interpreter::reservedirectiveToASM(const ts::Type& type)
{
    switch (type) {
        case ts::Type::int_t:
            [[fallthrough]];
        case ts::Type::float_t:
            return "resd";
        case ts::Type::bool_t:
            [[fallthrough]];
        case ts::Type::char_t:
            return "resb";
        default:
            error("cannot convert unknown type to assembly");
    }
}

void Interpreter::error(std::string_view msg)
{
    throw InterpretError(msg);
}
