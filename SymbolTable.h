#pragma once

#include <iterator>
#include <map>
#include <memory>
#include <stack>
#include <string>
#include <unordered_map>

#ifdef DEBUG
#include <iostream>
#endif

#include "Common.h"

#define SYMBOL_FLAG_INITIALIZED (1 << 0)
#define SYMBOL_FLAG_CONST       (1 << 1)
#define SYMBOL_FLAG_COMPILETIME (1 << 2)
#define SYMBOL_FLAG_USED        (1 << 3)

#define SYMBOL_SET_FLAG(s, f) (s.flags |= (f))
#define SYMBOL_GET_FLAG(s, f) (s.flags & (f))

struct Symbol
{
    ts::Type     type;
    std::uint8_t size; // size in bytes
    //
    // 000000001 - initialized
    // 000000010 - const
    // 000000100 - compile-time - can be calculated in compile time
    // 000001000 - used in expressions
    //
    std::uint8_t  flags  = 0;
    std::size_t   offset = 0; // offset in bytes from base of stack frame
    std::uint32_t value  = 0;
};

class Scope
{
public:
    Scope(std::shared_ptr<Scope> parent = nullptr)
    : m_symbols(),
      m_parent(parent),
      m_currentOffset(parent ? parent->m_currentOffset : 0)
    {
    }
    Scope(const Scope&) = delete;
    ~Scope() {}

    void insert(const std::string& name, const Symbol& sym);

    void makeGlobal() { m_global = true; }

    std::shared_ptr<Symbol> get(const std::string& name);
    std::shared_ptr<Scope>  getParent() const { return m_parent; }
    std::size_t             getOffset() const { return m_currentOffset; }
    std::size_t             getSize() const { return m_symbols.size(); }

    std::unordered_map<std::string, std::shared_ptr<Symbol>>::iterator begin() { return m_symbols.begin(); }
    std::unordered_map<std::string, std::shared_ptr<Symbol>>::iterator end() { return m_symbols.end(); }

private:
    std::unordered_map<std::string, std::shared_ptr<Symbol>> m_symbols; // symbols in this scope
    std::shared_ptr<Scope>                                   m_parent;
    std::size_t                                              m_currentOffset = 0;
    bool                                                     m_global        = false;

    // NOTE у всех областей видимости m_currentOffset будет наследоваться от родительской области.
};


class SymbolTable
{
public:
    SymbolTable() { m_currentScope = nullptr; }
    SymbolTable(SymbolTable&& o) : m_scopes(std::move(o.m_scopes)), m_currentScope(std::move(o.m_currentScope)){};
    ~SymbolTable() {}

    std::shared_ptr<Scope>& operator[](std::size_t id) { return m_scopes[id]; }

    void enterScope(std::size_t id);
    void exitScope();
    void insert(const std::string& name, const Symbol& sym);

    std::shared_ptr<Symbol> find(const std::string& name);
    bool                    inThisScope(const std::string& name);

    std::unordered_map<std::size_t, std::shared_ptr<Scope>>::iterator       begin() { return m_scopes.begin(); }
    std::unordered_map<std::size_t, std::shared_ptr<Scope>>::iterator       end() { return m_scopes.end(); }
    std::unordered_map<std::size_t, std::shared_ptr<Scope>>::const_iterator begin() const { return m_scopes.begin(); }
    std::unordered_map<std::size_t, std::shared_ptr<Scope>>::const_iterator end() const { return m_scopes.end(); }

private:
    std::unordered_map<std::size_t, std::shared_ptr<Scope>> m_scopes;
    std::shared_ptr<Scope>                                  m_currentScope;
};
