
#include "scheme.h"
#include "lexer.h"
#include "parser.h"

#define assert_eval(s,e,b) assert(s->eval(e)->toString() == b)

void test_tokenizer() {
    Lexer* l = new Lexer("(+ 1.5 (2 . \"\\\\\\aHej\\\"\") .x)");
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
    assert(l->getString() == "\\aHej\"");
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
    
    l = new Lexer("a `b");
    assert(l->nextToken() == Lexer::SYMBOL);
    assert(l->getString() == "a");
    assert(l->nextToken() == Lexer::BACKQUOTE);
    assert(l->nextToken() == Lexer::SYMBOL);
    assert(l->getString() == "b");
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
    assert(e->car->toString() == "quote");
    assert(e->cdrAsPair()->car->type() == SchemeObject::PAIR);
    assert(static_cast<SchemePair*>(e->cdrAsPair()->car)->car->type() == SchemeObject::SYMBOL);
    assert(static_cast<SchemePair*>(e->cdrAsPair()->car)->car->toString() == "x");
    assert(static_cast<SchemePair*>(e->cdrAsPair()->car)->cdr->toString() == "y");

    l = new Lexer("`(a b)");
    p = new Parser(l);
    t = p->parse();
    e = static_cast<SchemePair*> (t->car);
    assert(e->car->type() == SchemeObject::SYMBOL);
    assert(e->car->toString() == "quasiquote");
    assert(e->cdrAsPair()->car->type() == SchemeObject::PAIR);
    assert(static_cast<SchemePair*>(e->cdrAsPair()->car)->car->type() == SchemeObject::SYMBOL);
    assert(static_cast<SchemePair*>(e->cdrAsPair()->car)->car->toString() == "a");

}

void test_bools() {
    Scheme* s = new Scheme();
    assert(s->eval("(bool? #t)") == S_TRUE);
    assert(s->eval("(bool? #f)") == S_TRUE);
    assert(s->eval("(bool? 1)") == S_FALSE);
    assert(s->eval("(bool? '(1 2 3))") == S_FALSE);
}

void test_symbols() {
    Scheme* s = new Scheme();
    assert(s->eval("(symbol? (quote a))") == S_TRUE);
    assert(s->eval("(symbol? 'a)") == S_TRUE);
    assert(s->eval("(symbol? '1)") == S_FALSE);
    assert(s->eval("(symbol? '())") == S_FALSE);
    assert(s->eval("(symbol? 1)") == S_FALSE);
    assert(SchemeSymbol::create("a") == SchemeSymbol::create("a"));
    assert(SchemeSymbol::create("a") != SchemeSymbol::create("b"));
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

    // Test or and and
    assert_eval(s, "(and (= 2 2) (> 2 1))", "#t");
    assert_eval(s, "(and (= 2 2) (< 2 1))", "#f");
    assert_eval(s, "(and 1 2 'c '(f g))","(f g)");
    assert_eval(s, "(and)","#t");
    assert_eval(s, "(or)","#f");
    assert_eval(s, "(or (= 2 2) (> 2 1))", "#t");
    assert_eval(s, "(or (= 2 2) (< 2 1))", "#t");
    assert_eval(s, "(or #f #f #f)", "#f");
    assert_eval(s, "(or (member 'b '(a b c)) #f)", "(b c)");
    
    assert_eval(s, "(apply + (list 3 4))","7");
    assert_eval(s, "(apply + '(1 2 3))","6");
    assert_eval(s, "(apply + 1 2 '(3 4))", "10");
    assert_eval(s, "(apply list '())","()");
    assert_eval(s, "(apply * 1 2 (list 3 4))","24");
    s->eval("(define compose (lambda (f g) (lambda args (f (apply g args)))))");
    assert_eval(s, "((compose sqrt *) 12 75)","30");  // R^5RS, Section 6.4.
    
    delete s;
}

void test_math() {
    Scheme* s = new Scheme();
    assert_eval(s, "(- 3)" , "-3");
    assert_eval(s, "(- 3 2)" , "1");
    assert_eval(s, "(- 3 4 5)" , "-6");
    assert_eval(s, "(min 5)" , "5");
    assert_eval(s, "(min 3.0 1 2)" , "1");
    assert_eval(s, "(max 5)" , "5");
    assert_eval(s, "(max 3.0 1 2)" , "3");
    assert_eval(s, "(< 1 2 3)" , "#t");
    assert_eval(s, "(< 1 2 2 3)" , "#f");
    assert_eval(s, "(<= 1 2 2 3)" , "#t");
    assert_eval(s, "(> 3 2 1)" , "#t");
    assert_eval(s, "(> 1 2 2 3)" , "#f");
    assert_eval(s, "(>= 3 2 2 1)" , "#t");
    assert_eval(s, "(= 2 2 2 3)" , "#f");
    assert_eval(s, "(= 2 2 2 2)" , "#t");
}

void test_equals() {
    Scheme* s = new Scheme();
    assert_eval(s, "(equal? 1 1)" , "#t");
    assert_eval(s, "(equal? 1 2)" , "#f");
    assert_eval(s, "(equal? \"abc\" \"abc\")" , "#t");
    assert_eval(s, "(equal? '(1 2 3) '(1 2 3))" , "#t");
    assert_eval(s, "(equal? '(1 2 (a  b) 3) '(1 2 (a b) 3))" , "#t");
    assert_eval(s, "(equal? '(1 2 (a c) 3) '(1 2 (a b) 3))" , "#f");

    assert_eval(s, "(eq? 'a 'a)" , "#t");
    assert_eval(s, "(eq? (list 'a) (list 'a))" , "#f");
    assert_eval(s, "(eq? '() '())" , "#t");
    assert_eval(s, "(eq? car car)" , "#t");
    assert_eval(s, "(eq? (cons 1 2) (cons 1 2))" , "#f");

    assert_eval(s, "(eq? 'a 'a)" , "#t");
    assert_eval(s, "(eq? (list 'a) (list 'a))" , "#f");
    assert_eval(s, "(eq? '() '())" , "#t");
    assert_eval(s, "(eq? car car)" , "#t");
}

void test_pairs_and_lists() {
    SchemePair* p = s_cons(SchemeSymbol::create("x"),SchemeSymbol::create("y"));
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
    assert(s->eval("(pair? '#(a b))") == S_FALSE);    
    assert(s->eval("(null? '(1 2 3))") == S_FALSE);
    assert(s->eval("(null? '())") == S_TRUE);
    assert(s->eval("(null? 1)") == S_FALSE);
    
    assert_eval(s, "(cons 1 2)", "(1 . 2)");
    
    assert_eval(s, "(list)", "()");
    assert_eval(s, "(list 1)", "(1)");
    assert_eval(s, "(list '())", "(())");
    assert_eval(s, "(list 1 2 (+ 1 2) 4)", "(1 2 3 4)");

    assert_eval(s, "(car (cons 1 2))", "1");
    assert_eval(s, "(cdr (cons 1 2))", "2");
    assert_eval(s, "(cdr (list 1 2))", "(2)");

    assert_eval(s, "(reverse '(a (b c) d (e (f))))","((e (f)) d (b c) a)");
    
    assert_eval(s, "(member 3 '(1 2 3 4 5))", "(3 4 5)");
    assert_eval(s, "(member 10 '(1 2 3 4 5))", "#f");
    assert_eval(s, "(member 10 '())", "#f");
    assert_eval(s, "(member (list 'a) '(b (a) c))", "((a) c)");
    assert_eval(s, "(memq (list 'a) '(b (a) c))", "#f");
    
    assert_eval(s, "(list-tail '(1 2 3 4 5) 0)", "(1 2 3 4 5)");
    assert_eval(s, "(list-tail '(1 2 3 4 5) 1)", "(2 3 4 5)");
    assert_eval(s, "(list-tail '() 0)", "()");

    assert_eval(s, "(list-ref '(1 2 3) 0)", "1");
    assert_eval(s, "(list-ref '(1 2 3) 1)", "2");
    assert_eval(s, "(list-ref '(1 2 3) 2)", "3");
    
    assert_eval(s, "(append '() '(a b c) '(a b) '())", "(a b c a b)");
    assert_eval(s, "(append)", "()");
    assert_eval(s, "(append '() 'a)", "a");
    assert_eval(s, "(append 'a)", "a");
    assert_eval(s, "(append '(a b c) '(1 . 2))", "(a b c 1 . 2)"); // <-- error
    assert_eval(s, "(append '(a (b)) '((c)))", "(a (b) (c))");

    s->eval("(define e '((a 1) (b 2) (c 3)))");
    assert_eval(s, "(assq 'a e)", "(a 1)");
    assert_eval(s, "(assq 'b e)", "(b 2)");
    assert_eval(s, "(assq 'd e)", "#f");
    assert_eval(s, "(assoc (list 'a) '(((a)) ((b)) ((c))))", "((a))");
    assert_eval(s, "(assq (list 'a) '(((a)) ((b)) ((c))))", "#f");
    assert_eval(s, "(assv 5 '((2 3) (5 7) (11 13)))", "(5 7)");
    
    s->eval("(define e '(a b c d))");
    assert_eval(s, "(set-car! e 'f) e", "(f b c d)");
    assert_eval(s, "(set-cdr! e 'g) e", "(f . g)");
    assert_eval(s, "(set-cdr! e '()) e", "(f)");

    delete s;
}

void test_lambda() {
    Scheme* s = new Scheme();
    assert_eval(s, "(procedure? (lambda (x) x))", "#t");
    assert_eval(s, "(procedure? cons)", "#t");
    assert_eval(s, "(procedure? 1)", "#f");
    assert_eval(s, "(procedure? 'car)","#f");
    assert_eval(s, "(procedure? (lambda (x) (* x x)))","#t");
    assert_eval(s, "(procedure? '(lambda (x) (* x x)))","#f");
    assert_eval(s, "((lambda () 3))", "3");
    assert_eval(s, "((lambda (x) (* 2 x)) 10)", "20");
    assert_eval(s, "((lambda (x y) (+  y x)) 7 10)", "17");
    // Two examples from R^5RS
    assert_eval(s, "((lambda x x) 3 4 5 6)", "(3 4 5 6)");
    assert_eval(s, "((lambda (x y . z) z) 3 4 5 6)", "(5 6)");
}

void test_define_and_set() {
    Scheme* s = new Scheme();
    s->eval("(define x 17)");
    assert_eval(s, "x", "17");
    s->eval("(set! x 20)");
    assert_eval(s, "x", "20");
    s->eval("(define (square x) (* x x))");
    assert_eval(s, "(square 9)", "81");
    s->eval("(define (selftest . x) x)");
    // A R^5RS spec that guile 1.6.8 fails but we don't... :-)
    assert_eval(s, "(selftest 1 2 3 4)", "(1 2 3 4)");
    s->eval("(define (fact n) (if (equal? n 1) 1 (* n (fact (- n 1)))))");
    assert_eval(s, "(fact 6)", "720");
}

void test_string() {
    Scheme* s = new Scheme();
    assert_eval(s, "(string? 1)", "#f");
    assert_eval(s, "(string? \"\")", "#t");
    assert_eval(s, "(string? \"a\")", "#t");
    assert_eval(s, "(string? ((lambda () \"a\")))", "#t");
}

void test_begin() {
    Scheme* s = new Scheme();
    assert_eval(s, "(begin 1)", "1");
    assert_eval(s, "(begin 1 2)", "2");
    assert_eval(s, "(begin 1 2 3)", "3");
}

void test_quote() {
    Scheme* s = new Scheme();
    assert_eval(s, "'()", "()");
    assert_eval(s, "'(a b c)", "(a b c)");
    assert_eval(s, "'a", "a");
    assert_eval(s, "'1", "1");
    assert_eval(s, "(number? 1)", "#t");
    assert_eval(s, "(bool? '#t)", "#t");

    assert_eval(s, "`a", "a");
    assert_eval(s, "`()", "()");
    assert_eval(s, "`1", "1");
    assert_eval(s, "`(a b c)", "(a b c)");
    assert_eval(s, "`(a (+ 1 2) c)", "(a (+ 1 2) c)");
    assert_eval(s, "`(a ,(+ 1 2) c)", "(a 3 c)");
    assert_eval(s, "`(a ,1)", "(a 1)");

    assert_eval(s, "`(a ,(list 1 2 ) c)", "(a (1 2) c)");
    assert_eval(s, "`(a ,@(list 1 2 ) c)", "(a 1 2 c)");
}

void test_vector() {
    Scheme* s = new Scheme();
    SchemeVector* v = static_cast<SchemeVector*>(s->eval("(make-vector 3)"));
    assert(v->get(0) == S_UNSPECIFIED);
    assert(v->get(1) == S_UNSPECIFIED);
    assert(v->get(2) == S_UNSPECIFIED);
    assert_eval(s, "(make-vector 5 'a)", "#(a a a a a)");
    assert_eval(s, "(make-vector 2 (+ 5 1))", "#(6 6)");
    assert_eval(s, "(vector? (make-vector 5 'a))", "#t");
    assert_eval(s, "(vector-length (make-vector 7))","7");
    assert_eval(s, "(vector? 5)", "#f");
    assert_eval(s, "(vector 5 'a (+ 1 2) \"z\")", "#(5 a 3 \"z\")");
    assert_eval(s, "(vector)", "#()");
    assert_eval(s, "'#(a b c)", "#(a b c)");
    assert_eval(s, "(vector? (vector))", "#t");
    assert_eval(s, "(vector-length (vector))", "0");
    assert_eval(s, "(vector-length (vector 'a 'b))", "2");
    assert_eval(s, "(vector-length #())", "0");
    assert_eval(s, "(vector-length #(1 (1 2) 3))", "3");
    assert_eval(s, "(list->vector '(a b c))", "#(a b c)");
    assert_eval(s, "(list->vector '())", "#()");
    assert_eval(s, "(vector->list #(a b c))","(a b c)");
    assert_eval(s, "(vector-ref #(a b c) 1)","b");
    assert_eval(s, "(define v #(a b c))(vector-set! v 1 '(1 2 3))v","#(a (1 2 3) c)");
    assert_eval(s, "(define z (make-vector 4))(vector-fill! z 'a)z","#(a a a a)");
}

int main(int argc, char *argv[]) {
    try {
        cout << "Test tokenizer...       ";
        test_tokenizer();
        cout << " OK" << endl;
        cout << "Test parser...       ";
        test_parser();
        cout << " OK" << endl;
        
        cout << "Test interpreter...       ";
        test_interpreter();
        cout << " OK" << endl;
        
        cout << "Test bools...       ";
        test_bools();
        cout << " OK" << endl;
        
        cout << "Test symbols...       ";
        test_symbols();
        cout << " OK" << endl;

        cout << "Test equals...          ";
        test_equals();
        cout << " OK" << endl;

        cout << "Test math  ...          ";
        test_math();
        cout << " OK" << endl;


        cout << "Test vector...          ";
        test_vector();
        cout << " OK" << endl;

        cout << "Test pairs and lists... ";
        test_pairs_and_lists();
        cout << " OK" << endl;

        cout << "Test lambda...          ";
        test_lambda();
        cout << " OK" << endl;

        test_define_and_set();
        test_string();
        test_begin();
        test_quote();
    } catch (scheme_exception e) {
		cerr << "Exception: " << e.str << endl;
        return EXIT_FAILURE;
    }
    
    return EXIT_SUCCESS;
}
