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
    std::cout << "SemanticAnalyzer::buildSymbolTable() success\n";
    std::cout << "SemanticAnalyzer::typeCheck() called\n";
#endif

    traversalPostorder(node);

#ifdef DEBUG
    std::cout << "SemanticAnalyzer::typeCheck() success\n";
#endif
}

void SemanticAnalyzer::traversalPreorder(ast::ASTNodePtr& node)
{
    resolveId(node);

    for (ast::ASTNodePtr& c : node->getChildren()) {
        traversalPreorder(c);
    }
}

void SemanticAnalyzer::resolveId(ast::ASTNodePtr& node)
{
    std::visit(
        [&node, this](auto&& arg) -> void
        {
            using T = std::decay_t<decltype(arg)>;

            if constexpr (std::is_same_v<T, ast::Declaration>) {
                ast::Identifier* id = std::get_if<ast::Identifier>(&node->getChildren().front()->getData());

                ts::Type type = arg.getType();

                if (m_symbolTable.inThisScope(id->getName())) {
                    throw SemanticError(node->getLocation(),
                                        "symbol already declared in this "
                                        "scope");
                }

                Symbol s{type, ts::TypeSize[type], 0, 0, 0};

                // if id is initialized
                if (node->getChildren().size() == 2) {
                    SYMBOL_SET_FLAG(s, SYMBOL_FLAG_INITIALIZED);

                    ast::ASTNodePtr expr = node->getChildren().back();

                    if (compiletimeCalculated(expr)) {
                        SYMBOL_SET_FLAG(s, SYMBOL_FLAG_COMPILETIME);
                        ast::ASTNodePtr res = evaluate(expr);
                        node->replaceChild(node->getChildren().back(), res);

                        switch (type) {
                            case ts::Type::float_t:
                                s.value = valueToLong(ast::getValue<float>(res));
                                break;
                            case ts::Type::int_t:
                                s.value = ast::getValue<int>(res);
                                break;
                            case ts::Type::char_t:
                                [[fallthrough]];
                            case ts::Type::bool_t:
                                s.value = valueToLong(ast::getValue<std::uint8_t>(res));
                                break;
                            default:
                                break;
                        }
                    }
                }

                m_symbolTable.insert(id->getName(), s);
            }
            else if constexpr (std::is_same_v<T, ast::Identifier>) {
                std::string name = arg.getName();

                if (!m_symbolTable.find(name)) {
                    throw SemanticError(node->getLocation(), "unknown identifier: " + name);
                }
            }
            else if constexpr (std::is_same_v<T, ast::BlockStart>) {
                m_symbolTable.enterScope(arg.getScopeId());
            }
            else if constexpr (std::is_same_v<T, ast::BlockEnd>) {
                m_symbolTable.exitScope();
            }
            else if constexpr (std::is_same_v<T, ast::BinaryExpr>) {
                if (compiletimeCalculated(node)) {
                    ast::ASTNodePtr res    = evaluate(node);
                    ast::ASTNodePtr parent = node->getParent().lock();
                    parent->replaceChild(node, res);
                }
            }
        },
        node->getData());
}

void SemanticAnalyzer::traversalPostorder(ast::ASTNodePtr& node)
{
    for (ast::ASTNodePtr& c : node->getChildren()) {
        traversalPostorder(c);
    }

    resolveTypes(node);
}

ts::Type SemanticAnalyzer::getType(const ast::ASTNodePtr& node)
{
    return std::visit(
        [this](auto&& arg) -> ts::Type
        {
            using T = std::decay_t<decltype(arg)>;

            if constexpr (std::is_same_v<T, ast::Identifier>) {
                return m_symbolTable.find(arg.getName())->type;
            }
            else if constexpr (std::disjunction_v<std::is_same<T, ast::Integer>,
                                                  std::is_same<T, ast::Float>,
                                                  std::is_same<T, ast::Boolean>,
                                                  std::is_same<T, ast::BinaryExpr>>) {
                return arg.getType();
            }

            return ts::Type::unknown_t;
        },
        node->getData());
}

void SemanticAnalyzer::resolveTypes(ast::ASTNodePtr& node)
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
                                                               [this](auto n) { return ts::TypePrecedence[getType(n)]; });

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

                    bool equalTypes = getType(id) == getType(init);

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
                m_symbolTable.enterScope(arg.getScopeId());
            }
            else if constexpr (std::is_same_v<T, ast::BlockEnd>) {
                m_symbolTable.exitScope();
            }
        },
        node->getData());
}

ast::ASTNodePtr SemanticAnalyzer::evaluate(const ast::ASTNodePtr& node)
{
    return std::visit(
        [&node, this](auto&& x) -> ast::ASTNodePtr
        {
            using T = std::decay_t<decltype(x)>;

            if constexpr (std::is_same_v<T, ast::Integer> || std::is_same_v<T, ast::Float> ||
                          std::is_same_v<T, ast::Boolean> || std::is_same_v<T, ast::Identifier>) {
                return node;
            }
            if constexpr (std::is_same_v<T, ast::BinaryExpr>) {
                std::string op = x.getLiteral();

                std::cout << "Evaluating binary expression: " << op << std::endl;

                ast::ASTNodePtr left  = evaluate(node->getChildren().front());
                ast::ASTNodePtr right = evaluate(node->getChildren().back());

                ts::Type resultType = ts::Type::unknown_t;

                if (x.getType() == ts::Type::unknown_t) {
                    ts::Type lt = getType(left);
                    ts::Type rt = getType(right);
                    resultType  = (lt > rt) ? lt : rt;
                }
                else {
                    resultType = x.getType();
                }

                std::cout << "Result type: " << ts::TypeNames[resultType] << std::endl;

                ast::ASTNodePtr res = std::make_shared<ast::ASTNode>(ast::BinaryExpr(op, resultType));
                res->addChild(left);
                res->addChild(right);

                switch (resultType) {
                    case ts::Type::int_t:
                    {
                        return std::make_shared<ast::ASTNode>(ast::Integer(calculate<int>(res)));
                    }
                    case ts::Type::float_t:
                    {
                        return std::make_shared<ast::ASTNode>(ast::Float(calculate<float>(res)));
                    }
                    case ts::Type::bool_t:
                    {
                        // FIXME bool expressions calculating in float type
                        // it works, but it's a bit ugly
                        return std::make_shared<ast::ASTNode>(ast::Boolean(calculate<float>(res)));
                    }
                    case ts::Type::char_t:
                    {
                        return std::make_shared<ast::ASTNode>(ast::Integer(calculate<char>(res)));
                    }
                    default:
                        throw SemanticError("unknown type in binary expression");
                }
            }

            // non-reachable code
            return std::make_shared<ast::ASTNode>(ast::Integer(0));
        },
        node->getData());
}

bool SemanticAnalyzer::compiletimeCalculated(const ast::ASTNodePtr& node)
{
    return std::visit(
        [&node, this](auto&& x) -> bool
        {
            using T = std::decay_t<decltype(x)>;

            if constexpr (std::is_same_v<T, ast::Integer> || std::is_same_v<T, ast::Float> ||
                          std::is_same_v<T, ast::Boolean>) {
                return true;
            }
            if constexpr (std::is_same_v<T, ast::BinaryExpr>) {
                return compiletimeCalculated(node->getChildren().front()) &&
                       compiletimeCalculated(node->getChildren().back());
            }

            return false;
        },
        node->getData());
}
