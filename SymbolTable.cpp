#include "SymbolTable.h"

//=====----- Scope class -----=====//
void Scope::insert(const std::string& name, const Symbol& sym)
{
    m_symbols[name] = std::make_shared<Symbol>(sym);
}

std::shared_ptr<Symbol> Scope::get(const std::string& name)
{
    if (m_symbols.find(name) != m_symbols.end()) {
        return m_symbols[name];
    }

    return nullptr;
}

//=====----- SymbolTable class -----=====//
void SymbolTable::enterScope(void* scope)
{
    if (!m_scopes[scope]) {
        m_scopes[scope] = std::make_shared<Scope>(m_currentScope);
    }
    m_currentScope = m_scopes[scope];
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
    for (auto it = m_currentScope; it; it = it->getParent()) {
        if (auto sym = it->get(name)) {
            return sym;
        }
    }

    return nullptr;
}

bool SymbolTable::inThisScope(const std::string& name)
{
    return m_currentScope->get(name) != nullptr;
}
