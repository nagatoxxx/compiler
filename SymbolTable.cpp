#include "SymbolTable.h"
#include <iostream>

//=====----- Scope class -----=====//
void Scope::insert(const std::string& name, const Symbol& sym)
{
    m_symbols[name] = std::make_shared<Symbol>(sym);
    if (!m_global) {
        m_currentOffset += sym.size;
        m_symbols[name]->offset = m_currentOffset;
    }
}

std::shared_ptr<Symbol> Scope::get(const std::string& name)
{
    if (m_symbols.find(name) != m_symbols.end()) {
        return m_symbols[name];
    }

    return nullptr;
}

//=====----- SymbolTable class -----=====//
void SymbolTable::enterScope(std::size_t id)
{
    if (!m_scopes[id]) {
        m_scopes[id] = std::make_shared<Scope>(m_currentScope);
    }
    m_currentScope = m_scopes[id];
}

void SymbolTable::exitScope()
{
    m_currentScope = m_currentScope->getParent();
}

void SymbolTable::insert(const std::string& name, const Symbol& sym)
{
    if (!m_currentScope->get(name)) {
        m_currentScope->insert(name, sym);
    }
}

std::shared_ptr<Symbol> SymbolTable::find(const std::string& name)
{
    for (std::shared_ptr<Scope> it = m_currentScope; it; it = it->getParent()) {
        if (std::shared_ptr<Symbol> sym = it->get(name)) {
            return sym;
        }
    }

    return nullptr;
}

bool SymbolTable::inThisScope(const std::string& name)
{
    return m_currentScope->get(name) != nullptr;
}
