
#include "scheme.h"
#include "lexer.h"

void test_tokenizer() {
    Lexer* l = new Lexer("(+ 1.5 (2 \"Hej\"))");
    assert(l->nextToken() == Lexer::OPEN_PAREN);
    assert(l->nextToken() == Lexer::SYMBOL);
    assert(l->getString() == "+");
    assert(l->nextToken() == Lexer::NUMBER);
    assert(l->getNumber() == 1.5);
    assert(l->nextToken() == Lexer::OPEN_PAREN);
    assert(l->nextToken() == Lexer::NUMBER);
    assert(l->getNumber() == 2);
    assert(l->nextToken() == Lexer::STRING);
    assert(l->getString() == "Hej");
    assert(l->nextToken() == Lexer::CLOSE_PAREN);
    assert(l->nextToken() == Lexer::CLOSE_PAREN);
    assert(l->nextToken() == Lexer::END);
    delete l;

    l = new Lexer("#f #tf");
    assert(l->nextToken() == Lexer::BOOLEAN);
    assert(l->getBool() == false);
    assert(l->nextToken() == Lexer::BOOLEAN);
    assert(l->getBool() == true);
    assert(l->nextToken() == Lexer::SYMBOL);
    assert(l->nextToken() == Lexer::END);
    
    l = new Lexer("a");
    assert(l->nextToken() == Lexer::SYMBOL);
    assert(l->getString() == "a");
    assert(l->nextToken() == Lexer::END);
}

void test_parser() {
    
}


int main(int argc, char *argv[]) {
    test_tokenizer();
    test_parser();
    if (false) {
	    return EXIT_FAILURE;
    } else {
	    return EXIT_SUCCESS;
    }
}
