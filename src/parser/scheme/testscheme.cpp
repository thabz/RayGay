
#include "scheme.h"
#include "lexer.h"
#include "parser.h"

#define assert_eval(s,e,b) assert(s->eval(e)->toString() == b)

void test_tokenizer() {
    Lexer* l = new Lexer("(+ 1.5 (2 . \"Hej\") .x)");
    assert(l->nextToken() == Lexer::OPEN_PAREN);
    assert(l->nextToken() == Lexer::SYMBOL);
    assert(l->getString() == "+");
    assert(l->nextToken() == Lexer::NUMBER);
    assert(l->getNumber() == 1.5);
    assert(l->nextToken() == Lexer::OPEN_PAREN);
    assert(l->nextToken() == Lexer::NUMBER);
    assert(l->getNumber() == 2);
    assert(l->nextToken() == Lexer::PERIOD);
    assert(l->nextToken() == Lexer::STRING);
    assert(l->getString() == "Hej");
    assert(l->nextToken() == Lexer::CLOSE_PAREN);
    assert(l->nextToken() == Lexer::SYMBOL);
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
    
    l = new Lexer("'(x . y)");
    p = new Parser(l);
    t = p->parse();
    e = static_cast<SchemePair*> (t->car);
    assert(e->car->type() == SchemeObject::SYMBOL);
    assert(e->cdrAsPair()->car->type() == SchemeObject::PAIR);
    assert(static_cast<SchemePair*>(e->cdrAsPair()->car)->car->type() == SchemeObject::SYMBOL);
    assert(static_cast<SchemePair*>(e->cdrAsPair()->car)->car->toString() == "x");
    assert( static_cast<SchemePair*>(e->cdrAsPair()->car)->cdr->toString() == "y");
}

void test_bools() {
    Scheme* s = new Scheme();
    assert(s->eval("(bool? #t)") == S_TRUE);
    assert(s->eval("(bool? #f)") == S_TRUE);
    assert(s->eval("(bool? 1)") == S_FALSE);
    assert(s->eval("(bool? '(1 2 3))") == S_FALSE);
}

void test_interpreter() {
    // test eval_combo()
    Scheme* s = new Scheme();
    assert_eval(s, "((if #t reverse length) '(1 2 3))", "(3 2 1)");
    assert_eval(s, "((if #f reverse length) '(1 2 3))", "3");
    assert_eval(s, "((if #f reverse length) '(1 2 3))", "3");
    
    // test define
    assert_eval(s, "(define a 10) a", "10");
    
    // test built-in with only rst args
    assert_eval(s, "(+ 10 9 2 19 8 2 1 29 8 8 2 1 23 3 1) ", "126");
    delete s;
}

void test_equals() {
    Scheme* s = new Scheme();
    assert_eval(s, "(equal? 1 1)" , "#t");
    assert_eval(s, "(equal? 1 2)" , "#f");
    assert_eval(s, "(equal? \"abc\" \"abc\")" , "#t");
    assert_eval(s, "(equal? '(1 2 3) '(1 2 3))" , "#t");
    assert_eval(s, "(equal? '(1 2 (a  b) 3) '(1 2 (a b) 3))" , "#t");
    assert_eval(s, "(equal? '(1 2 (a c) 3) '(1 2 (a b) 3))" , "#f");
}

void test_pairs_and_lists() {
    SchemePair* p = s_cons(NULL,new SchemeSymbol("x"),new SchemeSymbol("y"));
    assert(p->toString() == "(x . y)");
    
    Scheme* s = new Scheme();
    assert(s->eval("(list? '())") == S_TRUE);
    assert(s->eval("(list? '(1 2 3))") == S_TRUE);
    assert(s->eval("(list? 1)") == S_FALSE);
    assert(s->eval("(list? '(1 2 . 3))") == S_FALSE);
    assert(s->eval("(pair? 1)") == S_FALSE);
    assert(s->eval("(pair? '())") == S_FALSE);
    assert(s->eval("(pair? '(1 2))") == S_TRUE);
    assert(s->eval("(pair? '(1 2 . 3))") == S_TRUE);
    
    assert_eval(s, "(cons 1 2)", "(1 . 2)");
    
    assert_eval(s, "(list)", "()");
    assert_eval(s, "(list 1)", "(1)");
    assert_eval(s, "(list '())", "(())");
    assert_eval(s, "(list 1 2 (+ 1 2) 4)", "(1 2 3 4)");

    assert_eval(s, "(member 3 '(1 2 3 4 5))", "(3 4 5)");
    assert_eval(s, "(member 10 '(1 2 3 4 5))", "#f");
    assert_eval(s, "(member 10 '())", "#f");

    assert_eval(s, "(list-tail '(1 2 3 4 5) 0)", "(1 2 3 4 5)");
    assert_eval(s, "(list-tail '(1 2 3 4 5) 1)", "(2 3 4 5)");
    assert_eval(s, "(list-tail '() 0)", "()");

    assert_eval(s, "(list-ref '(1 2 3) 0)", "1");
    assert_eval(s, "(list-ref '(1 2 3) 1)", "2");
    assert_eval(s, "(list-ref '(1 2 3) 2)", "3");

}

void test_lambda() {
    // Two examples from R^5RS
    assert_eval(s, "((lambda x x) 3 4 5 6)", "(3 4 5 6)");
    assert_eval(s, "((lambda (x y . z) z) 3 4 5 6)", "(5 6)");
}

int main(int argc, char *argv[]) {
    try {
        test_tokenizer();
        test_parser();
        test_interpreter();
        test_bools();
        test_equals();
        test_pairs_and_lists();
        //test_lambda();
    } catch (scheme_exception e) {
		cerr << "Exception: " << e.str << endl;
        return EXIT_FAILURE;
    }
    
    return EXIT_SUCCESS;
}
