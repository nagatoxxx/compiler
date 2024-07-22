#pragma once

#include <algorithm>
#include <any>
#include <iostream>
#include <list>
#include <memory>
#include <string>
#include <type_traits>
#include <variant>
#include <vector>

#include "Common.h"
#include "SymbolTable.h"

namespace ast
{
// classes representing AST nodes
class BinaryExpr
{
public:
    BinaryExpr(std::string_view literal) : m_literal(literal) {}
    ~BinaryExpr() {}

    std::string getLiteral() const { return m_literal; }
    ts::Type    getType() const { return m_resultType; }

    void setType(ts::Type type) { m_resultType = type; }

private:
    std::string m_literal; // string with operator
    ts::Type    m_resultType = ts::Type::unknown_t;
};

// unused
class UnaryExpr
{
public:
    UnaryExpr(std::string_view literal) : m_literal(literal) {}
    ~UnaryExpr() {}

    std::string getLiteral() const { return m_literal; }

private:
    std::string m_literal;
};

// variable declaration (then maybe function declaration)
class Declaration
{
public:
    Declaration(ts::Type t) : m_type(t) {}
    ~Declaration() {}

    ts::Type getType() const { return m_type; }

private:
    ts::Type m_type;
};

// variable, function, class name
class Identifier
{
public:
    Identifier(const std::string& name) : m_name(name) {}
    ~Identifier() {}

    std::string getName() const { return m_name; }

    ts::Type getType() const { return m_type; }
    void     setType(ts::Type type) { m_type = type; }

private:
    ts::Type    m_type;
    std::string m_name;
};

class Float
{
public:
    Float(float value) : m_value(value) {}
    ~Float() {}

    float    getValue() const { return m_value; }
    ts::Type getType() const { return m_type; }

private:
    float          m_value;
    const ts::Type m_type = ts::Type::float_t;
};

class Integer
{
public:
    Integer(int value) : m_value(value) {}
    ~Integer() {}

    int      getValue() const { return m_value; }
    ts::Type getType() const { return m_type; }

private:
    int            m_value;
    const ts::Type m_type = ts::Type::int_t;
};

// main node
class Root
{
public:
    Root() {}
    ~Root() {}
};

// if statement
// first child - condition
// second child - then body
// third child - else body
class Branch
{
public:
    Branch() {}
    ~Branch() {}
};

class Condition
{
public:
    Condition() {}
    ~Condition() {}
};

class BodyThen
{
public:
    BodyThen() {}
    ~BodyThen() {}
};

class BodyElse
{
public:
    BodyElse() {}
    ~BodyElse() {}
};

// unused
class Return
{
public:
    Return() {}
    ~Return() {}
};

class BlockStart
{
public:
    BlockStart() {}
    ~BlockStart() {}
};

// while loop statement
// first child - condition
// second child - body
class WhileLoop
{
public:
    WhileLoop() {}
    ~WhileLoop() {}
};

class BlockEnd
{
public:
    BlockEnd() {}
    ~BlockEnd() {}
};

class ImplicitTypeCast
{
public:
    ImplicitTypeCast(ts::Type from, ts::Type to) : m_toCast(to), m_fromCast(from) {}
    ~ImplicitTypeCast() {}

    ts::Type getToCast() const { return m_toCast; }
    ts::Type getFromCast() const { return m_fromCast; }

private:
    ts::Type m_toCast;
    ts::Type m_fromCast;
};

// for using statements
class ASTNode;

using ASTNodeData = std::
    variant<BinaryExpr, UnaryExpr, Float, Integer, Root, Declaration, Identifier, Branch, Condition, BodyThen, BodyElse, Return, BlockStart, BlockEnd, WhileLoop, ImplicitTypeCast>;
using ASTNodePtr  = std::shared_ptr<ASTNode>;
using ASTNodeWPtr = std::weak_ptr<ASTNode>;

class ASTNode : public std::enable_shared_from_this<ASTNode>
{
public:
    ASTNode(ASTNodeData type) : m_data(type) { std::fill(m_children.begin(), m_children.end(), nullptr); }
    ~ASTNode() {}

    ASTNodeData&           getData() noexcept { return m_data; }
    const Location&        getLocation() const noexcept { return m_loc; }
    ASTNodeWPtr            getParent() noexcept { return m_parent; }
    std::list<ASTNodePtr>& getChildren() noexcept { return m_children; }
    ASTNodePtr             ptr() { return shared_from_this(); }

    void setParent(const ASTNodePtr& parent) noexcept { m_parent = parent; }
    void setLocation(const Location& loc) noexcept { m_loc = loc; }

    [[maybe_unused]] ASTNodePtr& addChild(const ASTNodePtr& node) noexcept
    {
        node->setParent(ptr());
        m_children.push_back(node);
        return m_children.back();
    }
    [[maybe_unused]] void replaceChild(ASTNodePtr& old, ASTNodePtr& nw) noexcept
    {
        std::ranges::replace_if(
            m_children,
            [&old](const auto& n)
            {
                return n == old;
            },
            nw);
    }


private:
    ASTNodeData           m_data;
    ASTNodeWPtr           m_parent;
    std::list<ASTNodePtr> m_children;
    Location              m_loc;
};

#ifdef DEBUG
[[maybe_unused]] inline std::string ASTTypeToString(const ast::ASTNodePtr& node)
{
    return std::visit(
        [](auto&& arg) -> std::string
        {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, ast::BinaryExpr>) {
                return std::string("BinaryExpr: ") + arg.getLiteral() + " [ " + ts::TypeNames[arg.getType()] + " ]";
            }
            else if constexpr (std::is_same_v<T, ast::UnaryExpr>) {
                return std::string("UnaryExpr: ") + arg.getLiteral();
            }
            else if constexpr (std::is_same_v<T, ast::Float>) {
                return std::string("Float: ") + std::to_string(arg.getValue());
            }
            else if constexpr (std::is_same_v<T, ast::Integer>) {
                return std::string("Integer: ") + std::to_string(arg.getValue());
            }
            else if constexpr (std::is_same_v<T, ast::Root>) {
                return "Root";
            }
            else if constexpr (std::is_same_v<T, ast::Declaration>) {
                return "Declaration";
            }
            else if constexpr (std::is_same_v<T, ast::Identifier>) {
                return std::string("Identifier: ") + arg.getName();
            }
            else if constexpr (std::is_same_v<T, ast::Branch>) {
                return "Branch";
            }
            else if constexpr (std::is_same_v<T, ast::Condition>) {
                return "Condition";
            }
            else if constexpr (std::is_same_v<T, ast::BodyThen>) {
                return "BodyThen";
            }
            else if constexpr (std::is_same_v<T, ast::BodyElse>) {
                return "BodyElse";
            }
            else if constexpr (std::is_same_v<T, ast::Return>) {
                return "Return";
            }
            else if constexpr (std::is_same_v<T, ast::BlockStart>) {
                return "BlockStart";
            }
            else if constexpr (std::is_same_v<T, ast::BlockEnd>) {
                return "BlockEnd";
            }
            else if constexpr (std::is_same_v<T, ast::WhileLoop>) {
                return "WhileLoop";
            }
            else if constexpr (std::is_same_v<T, ast::ImplicitTypeCast>) {
                return std::string("ImplicitTypeCast: ") + ts::TypeNames[arg.getFromCast()] + std::string(" -> ") +
                       ts::TypeNames[arg.getToCast()];
            }
            else {
                return "None";
            }
        },
        node->getData());
}

static void PrintAST(const ast::ASTNodePtr& node, int indent = 0)
{
    for (int i = 0; i < indent; i++)
        std::cout << " ";
    std::cout << ASTTypeToString(node) << std::endl;
    for (const auto& c : node->getChildren()) {
        PrintAST(c, indent + 2);
    }
}
#endif

} // namespace ast
