
#include "lexer.h"
#include <iostream>
#include <sstream>

using namespace std;

int main(int argc, char *argv[]) {
    Lexer lexer = Lexer("(8000 122 1.2 (1.3) cons '() 19 \"Hello world\")");
    Lexer::Token token;
    do {
        token = lexer.nextToken();
        switch(token) {
            case Lexer::OPEN_PAREN :
               cout << "OPEN_PAREN" << endl;
               break;
            case Lexer::CLOSE_PAREN :
               cout << "CLOSE_PAREN" << endl;
               break;
            case Lexer::QUOTE :
               cout << "QUOTE" << endl;
               break;
            case Lexer::NUMBER :
               cout << "NUMBER (" << lexer.getNumber() << ")" << endl;
               break;
            case Lexer::STRING :
               cout << "STRING (" << lexer.getString() << ")" << endl;
               break;
            case Lexer::SYMBOL :
               cout << "SYMBOL (" << lexer.getString() << ")" << endl;
               break;
        }
    } while (token != Lexer::END);
}