
#include "scheme.h"
#include "lexer.h"
#include "parser.h"

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
    Lexer* l = new Lexer("(+ 1.5 (list? \"Hej\"))");
    Parser* p = new Parser(l);
    SchemePair* t = p->parse();
    SchemePair* e = static_cast<SchemePair*> (t->car);
    assert(e->car->type() == SchemeObject::SYMBOL);
    assert(e->cdrAsPair()->car->type() == SchemeObject::NUMBER);
    SchemePair* inner = static_cast<SchemePair*> (e->cdrAsPair()->cdrAsPair()->car);
    assert(inner->car->type() == SchemeObject::SYMBOL);
    assert(inner->car->toString() == "list?");
    assert(inner->cdrAsPair()->car->type() == SchemeObject::STRING);
    assert(inner->cdrAsPair()->cdrAsPair()->type() == SchemeObject::EMPTY_LIST);
    assert(e->cdrAsPair()->cdrAsPair()->cdrAsPair()->type() == SchemeObject::EMPTY_LIST);
}


int main(int argc, char *argv[]) {
    test_tokenizer();
    test_parser();
    return EXIT_SUCCESS;
}
