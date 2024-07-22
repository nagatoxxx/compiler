#include <algorithm>
#include <variant>

#ifdef DEBUG
#include <iostream>
#endif

#include "Common.h"
#include "SemanticAnalyzer.h"

void SemanticAnalyzer::analyze(ast::ASTNodePtr& node)
{
#ifdef DEBUG
    std::cout << "SemanticAnalyzer::buildSymbolTable() called\n";
#endif
    traversalPreorder(node);

#ifdef DEBUG
    std::cout << "SemanticAnalyzer::typeCheck() called\n\n";
#endif



    traversalPostorder(node);
#ifdef DEBUG
    ast::PrintAST(node);
    std::cout << "\nSemanticAnalyzer::typeCheck() success\n";
#endif
}

void SemanticAnalyzer::traversalPreorder(const ast::ASTNodePtr& node)
{
    flowControlCheck(node);

    for (const ast::ASTNodePtr& c : node->getChildren()) {
        traversalPreorder(c);
    }
}

void SemanticAnalyzer::IdResolution(const ast::ASTNodePtr& node)
{
    std::visit(
        [&node, this](auto&& arg) -> void
        {
            using T = std::decay_t<decltype(arg)>;

            if constexpr (std::is_same_v<T, ast::Declaration>) {
                const ast::Identifier* id = std::get_if<ast::Identifier>(&node->getChildren().front()->getData());

                ts::Type type = arg.getType();

                if (m_scopes.find(id->getName())) {
                    throw SemanticError(node->getLocation(),
                                        "symbol already declared in this "
                                        "scope");
                }

                m_scopes.insertSymbol(id->getName(), Symbol{type, ts::TypeSize[type]});
            }
            else if constexpr (std::is_same_v<T, ast::Identifier>) {
                std::string name = arg.getName();

                if (!m_scopes.find(name)) {
                    throw SemanticError(node->getLocation(), "unknown identifier: " + name);
                }
            }
            else if constexpr (std::is_same_v<T, ast::BlockStart>) {
                m_scopes.enterScope();
            }
            else if constexpr (std::is_same_v<T, ast::BlockEnd>) {
                m_scopes.exitScope();
            }
        },
        node->getData());
}

void SemanticAnalyzer::flowControlCheck(const ast::ASTNodePtr& node)
{
    std::visit(
        [&node, this](auto&& arg)
        {
            using T = std::decay_t<decltype(arg)>;

            if constexpr (std::is_same_v<T, ast::IfStatement>) {
                m_state.ifNodeVisited = true;
            }
            else if constexpr (std::is_same_v<T, ast::ElseStatement>) {
                if (!m_state.ifNodeVisited) {
                    throw SemanticError(node->getLocation(), "else statement must be preceded by an if statement");
                }
                m_state.ifNodeVisited = false;
            }
            // else if constexpr (std::is_same_v<T, ast::BlockStart>) {
            // }
            // else if constexpr (std::is_same_v<T, ast::BlockEnd>) {
            // }
        },
        node->getData());
}

void SemanticAnalyzer::traversalPostorder(ast::ASTNodePtr& node)
{
    for (ast::ASTNodePtr& c : node->getChildren()) {
        traversalPostorder(c);
    }

    // TODO переработать проверку типов
    typeCheck(node);
    IdResolution(node);
}

ts::Type SemanticAnalyzer::getType(const ast::ASTNodePtr& node)
{
    return std::visit(
        [this](auto&& arg) -> ts::Type
        {
            using T = std::decay_t<decltype(arg)>;

            if constexpr (std::is_same_v<T, ast::Identifier>) {
                return m_scopes.find(arg.getName())->type;
            }
            else if constexpr (std::disjunction_v<std::is_same<T, ast::Integer>,
                                                  std::is_same<T, ast::Float>,
                                                  std::is_same<T, ast::BinaryExpr>>) {
                return arg.getType();
            }

            return ts::Type::unknown_t;
        },
        node->getData());
}

void SemanticAnalyzer::typeCheck(ast::ASTNodePtr& node)
{
    std::visit(
        [&node, this](auto&& arg) mutable -> void
        {
            using T = std::decay_t<decltype(arg)>;

            if constexpr (std::is_same_v<T, ast::BinaryExpr>) {
                const ast::ASTNodePtr& left  = node->getChildren().front();
                const ast::ASTNodePtr& right = node->getChildren().back();

                // node with highest type priority
                ast::ASTNodePtr maxPriority = std::ranges::max(left,
                                                               right,
                                                               {},
                                                               [this](auto n)
                                                               {
                                                                   return ts::TypePrecedence[getType(n)];
                                                               });

                // node with lowest type priority
                ast::ASTNodePtr minPriority = (maxPriority == left) ? right : left;

                ts::Type maxType = getType(maxPriority);
                ts::Type minType = getType(minPriority);

                bool notEqualTypes;

                if ((notEqualTypes = maxType != minType) && !isImplicitlyCastable(minType, maxType)) {
                    throw SemanticError(node->getLocation(), "types do not match");
                }
                arg.setType(maxType);

                if (notEqualTypes) {
                    ast::ASTNodePtr parent = minPriority->getParent().lock();

                    ast::ASTNodePtr castNode = std::make_shared<ast::ASTNode>(ast::ImplicitTypeCast(minType, maxType));

                    parent->replaceChild(minPriority, castNode);

                    castNode->addChild(minPriority);
                    castNode->setLocation(minPriority->getLocation());
                }
            }
            // if statement, while statement conditions
            else if constexpr (std::is_same_v<T, ast::Condition>) {
                ast::ASTNodePtr expr = node->getChildren().front();

                if (std::holds_alternative<ast::BinaryExpr>(expr->getData())) {
                    ast::BinaryExpr data = std::get<ast::BinaryExpr>(expr->getData());

                    bool equalTypes = data.getType() == ts::Type::bool_t;

                    if (!(equalTypes || isImplicitlyCastable(data.getType(), ts::Type::bool_t))) {
                        throw SemanticError(expr->getLocation(), "condition must be boolean or convertible to boolean");
                    }

                    if (!equalTypes) {
                        ast::ASTNodePtr castNode = std::make_shared<ast::ASTNode>(
                            ast::ImplicitTypeCast(data.getType(), ts::Type::bool_t));

                        node->replaceChild(expr, castNode);

                        castNode->addChild(expr);
                        castNode->setLocation(expr->getLocation());
                    }
                }
            }
            else if constexpr (std::is_same_v<T, ast::Declaration>) {
                if (node->getChildren().size() == 2) {
                    ast::ASTNodePtr id   = node->getChildren().front();
                    ast::ASTNodePtr init = node->getChildren().back();

                    std::cout << "start\n";
                    bool equalTypes = getType(id) == getType(init);
                    std::cout << "end\n";

                    if (!(equalTypes || isImplicitlyCastable(getType(init), getType(id)))) {
                        throw SemanticError(node->getLocation(), "types do not match");
                    }

                    if (!equalTypes) {
                        ast::ASTNodePtr castNode = std::make_shared<ast::ASTNode>(
                            ast::ImplicitTypeCast(getType(init), getType(id)));

                        node->replaceChild(init, castNode);

                        castNode->addChild(init);
                        castNode->setLocation(init->getLocation());
                    }
                }
            }
            else if constexpr (std::is_same_v<T, ast::BlockStart>) {
                m_scopes.enterScope();
            }
            else if constexpr (std::is_same_v<T, ast::BlockEnd>) {
                m_scopes.exitScope();
            }
        },
        node->getData());
}