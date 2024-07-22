#include <fstream>
#include <iostream>
#include <sstream>

#include "Lexer.h"
#include "Parser.h"
#include "SemanticAnalyzer.h"

// prints a line of code with a caret indicating the error location
void printCodeLine(const Location& loc, const std::string& buf)
{
    std::stringstream ss(buf);
    std::string       tmp;

    std::size_t i = 0;

    while (i != loc.line && std::getline(ss, tmp)) {
        i++;
    }

    std::cout << tmp << '\n';

    for (std::size_t i = 1; i < loc.col; i++) {
        std::cout << ' ';
    }
    std::cout << "^\n";
}

int main(int argc, char* argv[])
{
    if (argc < 2) {
        std::cerr << "usage: " << argv[0] << " <filename>\n";
        return 1;
    }

    std::ifstream ifile(argv[1]);

    if (!ifile.is_open()) {
        std::cerr << "can't open " << argv[1] << '\n';
        return 1;
    }

    std::stringstream ss;
    ss << ifile.rdbuf();

    std::string buf(ss.str());

    try {
        Lexer            lexer;
        std::list<Token> tokens = lexer.tokenize(buf);

#ifdef DEBUG
        std::cout << "Tokens:\n";
        for (const Token& t : tokens) {
            std::cout << TokNames[t.getKind()] << '{' << t.getLoc().line << ' ' << t.getLoc().col << '}' << ' ';
        }
        std::cout << '\n';
#endif

        Parser          parser;
        ast::ASTNodePtr tree = parser.parse(tokens);

#ifdef DEBUG
        std::cout << "AST:\n";
        ast::PrintAST(tree);
        std::cout << '\n';
#endif

        SemanticAnalyzer sa;
        sa.analyze(tree);
    } catch (const LexicalError& e) {
        std::cerr << "Lexical error: " << e.what() << '\n';
        printCodeLine(e.getLocation(), buf);
    } catch (const SyntaxError& e) {
        std::cerr << "Syntax error: " << e.what() << '\n';
        printCodeLine(e.getLocation(), buf);
    } catch (const SemanticError& e) {
        std::cerr << "Semantic error: " << e.what() << '\n';
        printCodeLine(e.getLocation(), buf);
    }

    return 0;
}
