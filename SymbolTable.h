#pragma once

#include <cstdint>
#include <map>
#include <memory>
#include <stack>
#include <string>
#include <unordered_map>

#ifdef DEBUG
#include <iostream>
#endif

#include "Common.h"

struct Symbol
{
    ts::Type     type;
    std::uint8_t size; // size in bytes
};

class SymbolTable
{
public:
    SymbolTable() {}
    ~SymbolTable() = default;

    void insert(const std::string& name, const Symbol& symbol);

    std::shared_ptr<Symbol> find(const std::string& name);

private:
    std::unordered_map<std::string, std::shared_ptr<Symbol>> m_symbols;
};

struct Scope
{
    std::shared_ptr<SymbolTable> table;
    std::shared_ptr<Scope>       parentScope;

    Scope(std::shared_ptr<Scope> parent = nullptr) : table(std::make_shared<SymbolTable>()), parentScope(parent) {}
};

class Scopes
{
public:
    Scopes() : m_currentScope(std::make_shared<Scope>()) {}
    ~Scopes() = default;

    void enterScope();
    void exitScope();
    void insertSymbol(const std::string& name, const Symbol& symbol);

    std::shared_ptr<Symbol> find(const std::string& name);

private:
    std::shared_ptr<Scope> m_currentScope;
};
