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

class Scope
{
public:
    Scope(std::shared_ptr<Scope> parent = nullptr) : m_parent(parent) {}
    ~Scope() {}

    void insert(const std::string& name, const Symbol& sym);

#ifdef DEBUG
    void print()
    {
        for (auto& [name, sym] : m_symbols) {
            std::cout << name << ": " << sym << std::endl;
        }
    }
#endif

    std::shared_ptr<Symbol> get(const std::string& name);
    std::shared_ptr<Scope>  getParent() const { return m_parent; }

private:
    std::unordered_map<std::string, std::shared_ptr<Symbol>> m_symbols; // symbols in this scope
    std::shared_ptr<Scope>                                   m_parent;
};


class SymbolTable
{
public:
    SymbolTable() {}
    ~SymbolTable() {}

    void enterScope(void* scope);
    void exitScope();
    void insert(const std::string& name, const Symbol& sym);

#ifdef DEBUG
    void print()
    {
        for (const auto& [ptr, s] : m_scopes) {
            std::cout << ptr << ":\n";
            s->print();
        }
    }
#endif

    std::shared_ptr<Symbol> find(const std::string& name);
    bool                    inThisScope(const std::string& name);

private:
    std::unordered_map<void*, std::shared_ptr<Scope>> m_scopes;
    std::shared_ptr<Scope>                            m_currentScope;
};
