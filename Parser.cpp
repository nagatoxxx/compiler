#ifdef DEBUG
#include <iostream>
#endif

#include "AST.h"
#include "Common.h"
#include "Parser.h"

Parser::Parser() : m_ct(nullptr) {}

Parser::~Parser() {}


ast::ASTNodePtr Parser::parse(const std::list<Token>& tokens)
{
#ifdef DEBUG
    std::cout << "Parser::parse() called" << std::endl;
#endif
    m_ct = tokens.begin();

    auto tree = program();

#ifdef DEBUG
    std::cout << "Parser::parse() success" << std::endl;
#endif

    return tree;
}

void Parser::eat(const TokenKind& token)
{
    if (!m_ct->is(token)) {
        error();
    }
    m_ct++;
}

void Parser::eat()
{
    m_ct++;
}

ast::ASTNodePtr Parser::program()
{
    ast::ASTNodePtr tree = std::make_shared<ast::ASTNode>(ast::Root());
    while (m_ct->getKind() != TokenKind::EOS) {
        tree->addChild(statement());
    }

    eat(TokenKind::EOS);
    return tree;
}

ast::ASTNodePtr Parser::statement()
{
    TokenKind kind = m_ct->getKind();
    if (IS_TYPENAME(kind)) {
        return declaration_stmt();
    }
    else if (kind == TokenKind::KW_IF) {
        return branch_stmt();
    }
    else if (kind == TokenKind::KW_ELSE) {
        throw SemanticError("Else statement must be preceded by an if statement");
    }
    else if (kind == TokenKind::KW_WHILE) {
        return while_stmt();
    }
    else if (kind == TokenKind::IDENTIFIER) {
        return assignment_stmt();
    }
    else
        error();
}

ast::ASTNodePtr Parser::declaration_stmt()
{
    TokenKind kind = m_ct->getKind();

    ast::ASTNodePtr decl = std::make_shared<ast::ASTNode>(ast::Declaration(TOK_TO_TYPE(kind)));
    decl->setLocation(m_ct->getLoc());
    eat();

    if (m_ct->getKind() != TokenKind::IDENTIFIER) error();

    ast::Identifier id = ast::Identifier(m_ct->getString());
    id.setType(TOK_TO_TYPE(kind));

    decl->addChild(std::make_shared<ast::ASTNode>(id));
    decl->setLocation(m_ct->getLoc());

    eat();

    ast::ASTNodePtr ex = nullptr;
    if (m_ct->getKind() == TokenKind::ASSIGN) {
        eat();
        decl->addChild(expr());
    }
    eat(TokenKind::SEMI);

    return decl;
}

ast::ASTNodePtr Parser::branch_stmt()
{
    Location l = m_ct->getLoc();
    eat();
    eat(TokenKind::LPAREN);

    ast::ASTNodePtr cond = std::make_shared<ast::ASTNode>(ast::Condition());
    cond->addChild(expr());

    eat(TokenKind::RPAREN);

    ast::ASTNodePtr if_stmt = std::make_shared<ast::ASTNode>(ast::Branch());
    if_stmt->addChild(cond);
    if_stmt->addChild(body<ast::BodyThen>()); // then body
    if_stmt->setLocation(l);

    if (m_ct->getKind() == TokenKind::KW_ELSE) {
        eat();
        if_stmt->addChild(body<ast::BodyElse>()); // else body
    }

    return if_stmt;
}

ast::ASTNodePtr Parser::while_stmt()
{
    Location l = m_ct->getLoc();
    eat();
    eat(TokenKind::LPAREN);
    ast::ASTNodePtr cond = std::make_shared<ast::ASTNode>(ast::Condition());
    cond->addChild(expr());
    eat(TokenKind::RPAREN);

    ast::ASTNodePtr while_stmt = std::make_shared<ast::ASTNode>(ast::WhileLoop());
    while_stmt->addChild(cond);
    while_stmt->addChild(body<ast::BodyThen>());
    while_stmt->setLocation(l);

    return while_stmt;
}

ast::ASTNodePtr Parser::assignment_stmt()
{
    ast::ASTNodePtr id = std::make_shared<ast::ASTNode>(ast::Identifier(m_ct->getString()));
    id->setLocation(m_ct->getLoc());
    eat();
    eat(TokenKind::ASSIGN);
    ast::ASTNodePtr ex = expr();
    eat(TokenKind::SEMI);

    ast::ASTNodePtr be = std::make_shared<ast::ASTNode>(ast::BinaryExpr("="));
    be->addChild(id);
    be->addChild(ex);

    return be;
}

ast::ASTNodePtr Parser::expr()
{
    switch (m_ct->getKind()) {
        case TokenKind::INTEGER_CONSTANT:
            [[fallthrough]];
        case TokenKind::FLOATING_CONSTANT:
            [[fallthrough]];
        case TokenKind::IDENTIFIER:
            [[fallthrough]];
        case TokenKind::KW_TRUE:
            [[fallthrough]];
        case TokenKind::KW_FALSE:
            [[fallthrough]];
        case TokenKind::LPAREN:
            [[fallthrough]];
        case TokenKind::MINUS:
            return relation_expr();
        default:
            error(); // noreturn func
    }
}

ast::ASTNodePtr Parser::relation_expr()
{
    switch (m_ct->getKind()) {
        case TokenKind::INTEGER_CONSTANT:
            [[fallthrough]];
        case TokenKind::FLOATING_CONSTANT:
            [[fallthrough]];
        case TokenKind::IDENTIFIER:
            [[fallthrough]];
        case TokenKind::KW_TRUE:
            [[fallthrough]];
        case TokenKind::KW_FALSE:
            [[fallthrough]];
        case TokenKind::LPAREN:
            [[fallthrough]];
        case TokenKind::MINUS:
        {
            ast::ASTNodePtr left = additive_expr();
            ast::ASTNodePtr rt   = relation_tail(left);
            return rt ? rt : left;
        }
        default:
            error(); // noreturn func
    }
}

ast::ASTNodePtr Parser::relation_tail(const ast::ASTNodePtr& left)
{
    TokenKind kind = m_ct->getKind();

    if (kind == TokenKind::RPAREN || kind == TokenKind::RSQUARE || kind == TokenKind::SEMI) {
        return nullptr;
    }

    switch (m_ct->getKind()) {
        case TokenKind::GREATER:
        {
            eat();
            ast::ASTNodePtr additive = additive_expr();
            ast::ASTNodePtr rt       = relation_tail(additive);

            ast::ASTNodePtr op = std::make_shared<ast::ASTNode>(ast::BinaryExpr(">"));
            op->addChild(left);
            op->addChild(rt ? rt : additive);
            op->setLocation(m_ct->getLoc());

            return op;
        }
        case TokenKind::LESS:
        {
            eat();
            ast::ASTNodePtr additive = additive_expr();
            ast::ASTNodePtr rt       = relation_tail(additive);

            ast::ASTNodePtr op = std::make_shared<ast::ASTNode>(ast::BinaryExpr("<"));
            op->addChild(left);
            op->addChild(rt ? rt : additive);
            op->setLocation(m_ct->getLoc());

            return op;
        }
        case TokenKind::EQUAL:
        {
            eat();
            ast::ASTNodePtr additive = additive_expr();
            ast::ASTNodePtr rt       = relation_tail(additive);

            ast::ASTNodePtr op = std::make_shared<ast::ASTNode>(ast::BinaryExpr("=="));
            op->addChild(left);
            op->addChild(rt ? rt : additive);
            op->setLocation(m_ct->getLoc());

            return op;
        }
        case TokenKind::NE:
        {
            eat();
            ast::ASTNodePtr additive = additive_expr();
            ast::ASTNodePtr rt       = relation_tail(additive);

            ast::ASTNodePtr op = std::make_shared<ast::ASTNode>(ast::BinaryExpr("!="));
            op->addChild(left);
            op->addChild(rt ? rt : additive);
            op->setLocation(m_ct->getLoc());

            return op;
        }
        case TokenKind::GE:
        {
            eat();
            ast::ASTNodePtr additive = additive_expr();
            ast::ASTNodePtr rt       = relation_tail(additive);

            ast::ASTNodePtr op = std::make_shared<ast::ASTNode>(ast::BinaryExpr(">="));
            op->addChild(left);
            op->addChild(rt ? rt : additive);
            op->setLocation(m_ct->getLoc());

            return op;
        }
        case TokenKind::LE:
        {
            eat();
            ast::ASTNodePtr additive = additive_expr();
            ast::ASTNodePtr rt       = relation_tail(additive);

            ast::ASTNodePtr op = std::make_shared<ast::ASTNode>(ast::BinaryExpr("<="));
            op->addChild(left);
            op->addChild(rt ? rt : additive);
            op->setLocation(m_ct->getLoc());

            return op;
        }
        default:
            error();
    }
}

ast::ASTNodePtr Parser::additive_expr()
{
    switch (m_ct->getKind()) {
        case TokenKind::INTEGER_CONSTANT:
            [[fallthrough]];
        case TokenKind::FLOATING_CONSTANT:
            [[fallthrough]];
        case TokenKind::IDENTIFIER:
            [[fallthrough]];
        case TokenKind::KW_TRUE:
            [[fallthrough]];
        case TokenKind::KW_FALSE:
            [[fallthrough]];
        case TokenKind::LPAREN:
            [[fallthrough]];
        case TokenKind::MINUS:
        {
            ast::ASTNodePtr left = multiplicative_expr();
            ast::ASTNodePtr mt   = additive_tail(left);
            return mt ? mt : left;
        }
        default:
            error(); // noreturn func
    }
}

ast::ASTNodePtr Parser::additive_tail(const ast::ASTNodePtr& left)
{
    TokenKind kind = m_ct->getKind();
    if (IS_RELOP(kind) || kind == TokenKind::RSQUARE || kind == TokenKind::RPAREN || kind == TokenKind::SEMI) {
        return nullptr;
    }

    switch (kind) {
        case TokenKind::MINUS:
        {
            eat();
            ast::ASTNodePtr mult = multiplicative_expr();
            ast::ASTNodePtr at   = additive_tail(mult);

            ast::ASTNodePtr op = std::make_shared<ast::ASTNode>(ast::BinaryExpr("-"));
            op->addChild(left);
            op->addChild(at ? at : mult);
            op->setLocation(m_ct->getLoc());

            return op;
        }
        case TokenKind::PLUS:
        {
            eat();
            ast::ASTNodePtr mult = multiplicative_expr();
            ast::ASTNodePtr at   = additive_tail(mult);

            ast::ASTNodePtr op = std::make_shared<ast::ASTNode>(ast::BinaryExpr("+"));
            op->addChild(left);
            op->addChild(at ? at : mult);
            op->setLocation(m_ct->getLoc());

            return op;
        }
        default:
            error();
    }
}

ast::ASTNodePtr Parser::multiplicative_expr()
{
    switch (m_ct->getKind()) {
        case TokenKind::INTEGER_CONSTANT:
            [[fallthrough]];
        case TokenKind::FLOATING_CONSTANT:
            [[fallthrough]];
        case TokenKind::IDENTIFIER:
            [[fallthrough]];
        case TokenKind::KW_TRUE:
            [[fallthrough]];
        case TokenKind::KW_FALSE:
            [[fallthrough]];
        case TokenKind::LPAREN:
            [[fallthrough]];
        case TokenKind::MINUS:
        {
            ast::ASTNodePtr left = unary_expr();
            ast::ASTNodePtr mt   = multiplicative_tail(left);
            return mt ? mt : left;
        }
        default:
            error(); // noreturn func
    }
}

ast::ASTNodePtr Parser::multiplicative_tail(const ast::ASTNodePtr& left)
{
    TokenKind kind = m_ct->getKind();

    if (IS_RELOP(kind) || kind == TokenKind::RSQUARE || kind == TokenKind::RPAREN || kind == TokenKind::PLUS ||
        kind == TokenKind::MINUS || kind == TokenKind::SEMI) {
        return nullptr;
    }

    switch (kind) {
        case TokenKind::STAR:
        {
            eat();
            ast::ASTNodePtr ue = unary_expr();
            ast::ASTNodePtr mt = multiplicative_tail(ue);

            ast::ASTNodePtr op = std::make_shared<ast::ASTNode>(ast::BinaryExpr("*"));
            op->addChild(left);
            op->addChild(mt ? mt : ue);
            op->setLocation(m_ct->getLoc());

            return op;
        }
        case TokenKind::SLASH:
        {
            eat();
            ast::ASTNodePtr ue = unary_expr();
            ast::ASTNodePtr mt = multiplicative_tail(ue);

            ast::ASTNodePtr op = std::make_shared<ast::ASTNode>(ast::BinaryExpr("/"));
            op->addChild(left);
            op->addChild(mt ? mt : ue);
            op->setLocation(m_ct->getLoc());

            return op;
        }
        default:
            error();
    }
}

ast::ASTNodePtr Parser::unary_expr()
{
    switch (m_ct->getKind()) {
        case TokenKind::MINUS:
        {
            eat();
            ast::ASTNodePtr op = std::make_shared<ast::ASTNode>(ast::UnaryExpr("-"));
            op->addChild(unary_expr());
            op->setLocation(m_ct->getLoc());

            return op;
        }
        case TokenKind::LPAREN:
        {
            return access_expr();
        }
        case TokenKind::INTEGER_CONSTANT:
        {
            return access_expr();
        }
        case TokenKind::FLOATING_CONSTANT:
        {
            return access_expr();
        }
        case TokenKind::IDENTIFIER:
        {
            return access_expr();
        }
        case TokenKind::KW_TRUE:
        {
            return access_expr();
        }
        case TokenKind::KW_FALSE:
        {
            return access_expr();
        }
        default:
            error();
    }
}

ast::ASTNodePtr Parser::access_expr()
{

    switch (m_ct->getKind()) {
        case TokenKind::IDENTIFIER:
            [[fallthrough]];
        case TokenKind::LPAREN:
            [[fallthrough]];
        case TokenKind::INTEGER_CONSTANT:
            [[fallthrough]];
        case TokenKind::KW_TRUE:
            [[fallthrough]];
        case TokenKind::KW_FALSE:
            [[fallthrough]];
        case TokenKind::FLOATING_CONSTANT:
        {
            ast::ASTNodePtr pr = primary();
            ast::ASTNodePtr at = access_tail(pr);

            return at ? at : pr;
        }
        default:
            error();
    }
}

ast::ASTNodePtr Parser::access_tail(const ast::ASTNodePtr& left)
{
    TokenKind kind = m_ct->getKind();
    if (IS_RELOP(kind) || kind == TokenKind::RSQUARE || kind == TokenKind::RPAREN || kind == TokenKind::PLUS ||
        kind == TokenKind::MINUS || kind == TokenKind::STAR || kind == TokenKind::SLASH || kind == TokenKind::SEMI) {
        return nullptr;
    }

    switch (kind) {
        case TokenKind::PERIOD:
        {
            eat();
            ast::ASTNodePtr id = std::make_shared<ast::ASTNode>(ast::Identifier(m_ct->getString()));
            eat(TokenKind::IDENTIFIER);
            ast::ASTNodePtr at = access_tail(id);

            ast::ASTNodePtr op = std::make_shared<ast::ASTNode>(ast::BinaryExpr("."));
            op->addChild(left);
            op->addChild(at ? at : id);
            op->setLocation(m_ct->getLoc());

            return op;
        }
        case TokenKind::LSQUARE:
        {
            eat();
            ast::ASTNodePtr ex = expr();
            eat(TokenKind::RSQUARE);
            ast::ASTNodePtr at = access_tail(ex);

            ast::ASTNodePtr op = std::make_shared<ast::ASTNode>(ast::BinaryExpr("[]"));
            op->addChild(left);
            op->addChild(at ? at : ex);
            op->setLocation(m_ct->getLoc());

            return op;
        }
        default:
            error();
    }
}

ast::ASTNodePtr Parser::primary()
{
    switch (m_ct->getKind()) {
        case TokenKind::INTEGER_CONSTANT:
        {
            ast::ASTNodePtr integer = std::make_shared<ast::ASTNode>(ast::Integer(m_ct->getInteger()));
            integer->setLocation(m_ct->getLoc());
            eat();
            return integer;
        }
        case TokenKind::FLOATING_CONSTANT:
        {
            ast::ASTNodePtr floating = std::make_shared<ast::ASTNode>(ast::Float(m_ct->getFloat()));
            floating->setLocation(m_ct->getLoc());
            eat();
            return floating;
        }
        case TokenKind::IDENTIFIER:
        {
            ast::ASTNodePtr identifier = std::make_shared<ast::ASTNode>(ast::Identifier(m_ct->getString()));
            identifier->setLocation(m_ct->getLoc());
            eat();
            return identifier;
        }
        case TokenKind::LPAREN:
        {
            eat();
            ast::ASTNodePtr ex = expr();
            eat(TokenKind::RPAREN);
            return ex;
        }
        case TokenKind::KW_TRUE:
        {
            ast::ASTNodePtr true_ = std::make_shared<ast::ASTNode>(ast::Boolean(true));
            true_->setLocation(m_ct->getLoc());
            eat();
            return true_;
        }
        case TokenKind::KW_FALSE:
        {
            ast::ASTNodePtr false_ = std::make_shared<ast::ASTNode>(ast::Boolean(false));
            false_->setLocation(m_ct->getLoc());
            eat();
            return false_;
        }
        default:
            error();
    }
}

void Parser::error()
{
    throw SyntaxError(m_ct->getLoc());
}
