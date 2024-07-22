#include "SymbolTable.h"

//=====----- SymbolTable class -----=====//
void SymbolTable::insert(const std::string& name, const Symbol& symbol)
{
    m_symbols[name] = std::make_shared<Symbol>(symbol);
}

std::shared_ptr<Symbol> SymbolTable::find(const std::string& name)
{
    auto it = m_symbols.find(name);
    if (it != m_symbols.end()) {
        return it->second;
    }
    return nullptr;
}

//=====----- Scopes class -----=====//
void Scopes::enterScope()
{
    m_currentScope = std::make_shared<Scope>(m_currentScope);
}

void Scopes::exitScope()
{
    if (m_currentScope) {
        m_currentScope = m_currentScope->parentScope;
    }
}

void Scopes::insertSymbol(const std::string& name, const Symbol& symbol)
{
    if (m_currentScope) {
        m_currentScope->table->insert(name, symbol);
    }
}

std::shared_ptr<Symbol> Scopes::find(const std::string& name)
{
    std::shared_ptr<Scope> scope = m_currentScope;

    while (scope) {
        std::shared_ptr<Symbol> symbol = scope->table->find(name);
        if (symbol) {
            return symbol;
        }
        scope = scope->parentScope;
    }

    return nullptr;
}
