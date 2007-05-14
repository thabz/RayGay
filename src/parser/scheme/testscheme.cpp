
#include "scheme.h"
#include "lexer.h"
#include "interpreter.h"
#include "parser.h"
#include <sstream>
#include <exception>

/*
assert_eval(s, "", "");
assert_eval(s, "", "");
assert_eval(s, "", "");
assert_eval(s, "", "");
assert_eval(s, "", "");
assert_eval(s, "", "");
assert_eval(s, "", "");
assert_eval(s, "", "");
assert_eval(s, "", "");
assert_eval(s, "", "");
assert_eval(s, "", "");
assert_eval(s, "", "");
*/

int errors_found = 0;

void assert_eval(Scheme* s, string expression, string expected) {
    if (expression == "") {
        return;
    }
    
    uint stacksize_before = stack.size();

    try {
//        cout << "Testing " << expression << endl;
        string result = s->eval(expression)->toString();
        if (result != expected) {
            errors_found++;
            cerr << "FAILED: " << expression;
            cerr << " expected " << expected;
            cerr << " got " << result << endl; 
        }  
    } catch (scheme_exception e) {
        errors_found++;
        cerr << "FAILED: " << expression << ": " << e.str << endl; 
    } catch (exception e) {
        errors_found++;
        cerr << "FAILED: " << expression << ": general exception" << endl; 
    }
    if (stacksize_before != stack.size()) {
        cerr << "FAILED: " << expression << " made stack explode" << endl;
    }
}

void assert_fail(Scheme* s, string expression) {
    uint stacksize_before = stack.size();
    try {
        s->eval(expression);
        errors_found++;
        cerr << "FAILED: " << expression;
        cerr << " didn't fail" << endl;
    } catch (scheme_exception e) {
        if (stacksize_before != stack.size()) {
            cerr << "FAILED: stack exploded when " << expression << " failed." << endl;
        }
    }
}

void test_tokenizer() {
    istream* is = new istringstream("(+ 1.5 (2 . \"\\\\\\aHej\\\"\") .x)");
    Lexer* l = new Lexer();
    assert(l->nextToken(is) == Lexer::OPEN_PAREN);
    assert(l->nextToken(is) == Lexer::SYMBOL);
    assert(l->getString() == "+");
    assert(l->nextToken(is) == Lexer::NUMBER);
    assert(l->getNumber() == 1.5);
    assert(l->nextToken(is) == Lexer::OPEN_PAREN);
    assert(l->nextToken(is) == Lexer::NUMBER);
    assert(l->getNumber() == 2);
    assert(l->nextToken(is) == Lexer::PERIOD);
    assert(l->nextToken(is) == Lexer::STRING);
    assert(l->getString() == "\\aHej\"");
    assert(l->nextToken(is) == Lexer::CLOSE_PAREN);
    assert(l->nextToken(is) == Lexer::SYMBOL);
    assert(l->nextToken(is) == Lexer::CLOSE_PAREN);
    assert(l->nextToken(is) == Lexer::END);
    delete is;
    
    is = new istringstream("#f #tf");
    assert(l->nextToken(is) == Lexer::BOOLEAN);
    assert(l->getBool() == false);
    assert(l->nextToken(is) == Lexer::BOOLEAN);
    assert(l->getBool() == true);
    assert(l->nextToken(is) == Lexer::SYMBOL);
    assert(l->nextToken(is) == Lexer::END);
    delete is;
    
    is = new istringstream("a `b #| comment #| nested comment |# ... |# ");
    assert(l->nextToken(is) == Lexer::SYMBOL);
    assert(l->getString() == "a");
    assert(l->nextToken(is) == Lexer::BACKQUOTE);
    assert(l->nextToken(is) == Lexer::SYMBOL);
    assert(l->getString() == "b");
    assert(l->nextToken(is) == Lexer::END);
    delete is;

    delete l;
}


void test_objects() {
    SchemeObject* n = SchemeObject::createNumber(1.0);
    assert(n->type() == SchemeObject::NUMBER);
    assert(n->immutable() == false);
    n->set_immutable(true);
    assert(n->immutable() == true);
    assert(n->type() == SchemeObject::NUMBER);
    n->set_immutable(false);
    assert(n->immutable() == false);
    assert(n->type() == SchemeObject::NUMBER);
}

void test_parser() {
    istream* is = new istringstream("(+ 1.5 (list? \"Hej\"))");
    Parser* p = new Parser();
    SchemeObject* t = p->parse(is);
    SchemeObject* e = s_car(t);
    assert(s_car(e)->type() == SchemeObject::SYMBOL);
    assert(s_cadr(e)->type() == SchemeObject::NUMBER);
    SchemeObject* inner = s_caddr(e);
    assert(s_car(inner)->type() == SchemeObject::SYMBOL);
    assert(s_car(inner)->toString() == "list?");
    assert(s_cadr(inner)->type() == SchemeObject::STRING);
    assert(s_cddr(inner)->type() == SchemeObject::EMPTY_LIST);
    assert(s_cdddr(e)->type() == SchemeObject::EMPTY_LIST);
    
    is = new istringstream("'(x . y)");
    t = p->parse(is);
    e = s_car(t);
    assert(s_car(e)->type() == SchemeObject::SYMBOL);
    assert(s_car(e)->toString() == "quote");
    assert(s_cadr(e)->type() == SchemeObject::PAIR);
    assert(s_caadr(e)->type() == SchemeObject::SYMBOL);
    assert(s_caadr(e)->toString() == "x");
    assert(s_cdadr(e)->toString() == "y");

    is = new istringstream("`(a b)");
    t = p->parse(is);
    e = s_car(t);
    assert(s_car(e)->type() == SchemeObject::SYMBOL);
    assert(s_car(e)->toString() == "quasiquote");
    assert(s_cadr(e)->type() == SchemeObject::PAIR);
    assert(s_caadr(e)->type() == SchemeObject::SYMBOL);
    assert(s_caadr(e)->toString() == "a");
}

void test_interpreter() {
    Scheme* s = new Scheme();
    assert(s->eval("") == S_UNSPECIFIED);
    assert(s->eval("#| xxx |#") == S_UNSPECIFIED);
    assert(s->eval("#| xxx |# ") == S_UNSPECIFIED);
    assert(s->eval(";xxxxx ") == S_UNSPECIFIED);

    // test eval_combo()
    assert_eval(s, "((if #t reverse length) '(1 2 3))", "(3 2 1)");
    assert_eval(s, "((if #f reverse length) '(1 2 3))", "3");
    assert_eval(s, "((if #f reverse length) '(1 2 3))", "3");
    assert_eval(s, "((if #f + *) 3 4)", "12");
    assert_fail(s, "(if)");
    assert_fail(s, "(if #t)");
    
    // test define
    assert_eval(s, "(define a 10) a", "10");
    assert_fail(s, "(define)");
    assert_fail(s, "(define a)");
    assert_fail(s, "(define 10 10)");

    // test built-in with only rst args
    assert_eval(s, "(+ 10 9 2 19 8 2 1 29 8 8 2 1 23 3 1) ", "126");
    assert_eval(s, "(+ (+ 1 2 3) (+ 1 2 3)) ", "12");

    // Test or and and
    assert_eval(s, "(and (= 2 2) (> 2 1))", "#t");
    assert_eval(s, "(and (= 2 2) (< 2 1))", "#f");
    assert_eval(s, "(and 1 2 'c '(f g))","(f g)");
    assert_eval(s, "(and)","#t");
    assert_eval(s, "(or)","#f");
    assert_eval(s, "(or (= 2 2) (> 2 1))", "#t");
    assert_eval(s, "(or (= 2 2) (< 2 1))", "#t");
    assert_eval(s, "(or #f #f #f)", "#f");
    assert_eval(s, "(or (member 2 '(1 2 3)) #f)", "(2 3)");
    assert_eval(s, "(or (member 'b '(a b c)) #f)", "(b c)");
    assert_eval(s, "(not #t)","#f");
    assert_eval(s, "(not 3)","#f");
    assert_eval(s, "(not (list 3))","#f");
    assert_eval(s, "(not #f)","#t");
    assert_eval(s, "(not '())","#f");
    assert_eval(s, "(not (list))","#f");
    assert_eval(s, "(not 'nil)","#f");
    assert_fail(s, "(not)");

    assert_eval(s, "(apply + (list 3 4))","7");
    assert_eval(s, "(apply + '(1 2 3))","6");
    assert_eval(s, "(apply + 1 2 '(3 4))", "10");
    assert_eval(s, "(apply list '())","()");
    assert_eval(s, "(apply * 1 2 (list 3 4))","24");
    assert_eval(s, "(apply apply `(,+ ,(list 1 2)))", "3");
    s->eval("(define compose (lambda (f g) (lambda args (f (apply g args)))))");
    assert_eval(s, "((compose sqrt *) 12 75)","30");  // R^5RS, Section 6.4.
    
    assert_eval(s, "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))", "composite");
    assert_eval(s, "(case (car '(c d)) ((a) 'a) ((b) 'b))", "#<unspecified>");
    assert_eval(s, "(case (car '(c d)) ((a e i o u) 'vowel) ((w y) 'semivowel) (else 'consonant))", "consonant");
    assert_eval(s, "(case 2)", "#<unspecified>");
    assert_fail(s, "(case)");
    assert_fail(s, "(case 2 (2))");
    assert_fail(s, "(case 2 2)");
    
    assert_eval(s, "(cond ((> 3 2) 'greater) ((< 3 2) 'less))","greater");
    assert_eval(s, "(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))", "equal");
    assert_eval(s, "(cond ((assv 'b '((a 1) (b 2))) => cadr) (else #f))", "2");
    assert_eval(s, "(cond ((equal? 'a 'a)) (else 'b))", "#t");
    assert_eval(s, "(cond)","#<unspecified>");
    assert_eval(s, "(cond (17))","17");
    assert_eval(s, "(cond ((> 3 2)))", "#t");
    assert_fail(s, "(cond a)");
    assert_eval(s, "(cond ('a 'b))","b");
    
    // Brian M. Moore in thread: shadowing syntatic keywords, bug in MIT Scheme?
    // http://groups.google.com/groups?selm=6e6n88%248qf%241%40news.cc.ukans.edu        
    assert_eval(s, "((lambda lambda lambda) 'x)", "(x)");
    assert_eval(s, "(let ((quote -)) (eqv? '1 1))", "#f");
    assert_eval(s, "((lambda (begin) (begin 1 2 3)) (lambda lambda lambda))", "(1 2 3)");    

    delete s;
}

void test_bools() {
    Scheme* s = new Scheme();
    assert_eval(s, "(boolean? #t)", "#t");
    assert_eval(s, "(boolean? #f)", "#t");
    assert_eval(s, "(boolean? 1)", "#f");
    assert_eval(s, "(boolean? '(1 2 3))", "#f");
}

void test_char() {
    Scheme* s = new Scheme();
    assert_eval(s, "(char? #\\a)", "#t");
    assert_eval(s, "(char? 1)", "#f");
    assert_eval(s, "#\\b", "#\\b");
    assert_eval(s, "(char->integer #\\Space)", "32");
    assert_eval(s, "(char->integer #\\newline)", "10");
    assert_eval(s, "#\\space", "#\\space");
    assert_eval(s, "#\\newline", "#\\newline");
    assert_eval(s, "(integer->char 66)", "#\\B");
    assert_eval(s, "(integer->char 95)", "#\\_");
    assert_fail(s, "(integer->char -1)");
    assert_fail(s, "(integer->char 'a)");
    assert_eval(s, "(char->integer #\\B)", "66");
    assert_eval(s, "(char->integer #\\_)", "95");
    assert_eval(s, "(<= (char->integer #\\a) (char->integer #\\b))", "#t");
    assert_eval(s, "(char-upcase #\\a)", "#\\A");
    assert_eval(s, "(char-upcase #\\A)", "#\\A");
    assert_eval(s, "(char-downcase #\\b)", "#\\b");
    assert_eval(s, "(char-downcase #\\B)", "#\\b");
    assert_eval(s, "(char-alphabetic? #\\a)", "#t");
    assert_eval(s, "(char-alphabetic? #\\space)", "#f");
    assert_eval(s, "(char-alphabetic? #\\1)", "#f");
    assert_eval(s, "(char-numeric? #\\1)", "#t");
    assert_eval(s, "(char-numeric? #\\x)", "#f");
    assert_eval(s, "(char-whitespace? #\\ )", "#t");
    assert_eval(s, "(char-whitespace? #\\a)", "#f");
    assert_eval(s, "(char-whitespace? #\\3)", "#f");
    assert_eval(s, "(char-upper-case? #\\F)", "#t");
    assert_eval(s, "(char-upper-case? #\\f)", "#f");
    assert_eval(s, "(char-upper-case? #\\1)", "#f");
    assert_eval(s, "(char-lower-case? #\\F)", "#f");
    assert_eval(s, "(char-lower-case? #\\f)", "#t");
    assert_eval(s, "(char-lower-case? #\\1)", "#f");
}

void test_symbols() {
    Scheme* s = new Scheme();
    assert_eval(s, "(symbol? (quote a))","#t");
    assert(s->eval("(symbol? 'a)") == S_TRUE);
    assert(s->eval("(symbol? '1)") == S_FALSE);
    assert(s->eval("(symbol? '())") == S_FALSE);
    assert(s->eval("(symbol? 1)") == S_FALSE);
    assert(SchemeObject::createSymbol("a") == SchemeObject::createSymbol("a"));
    assert(SchemeObject::createSymbol("a") != SchemeObject::createSymbol("b"));
    assert_eval(s, "(eq? (string->symbol \"f\") (string->symbol \"F\"))", "#f");
}

void test_math() {
    Scheme* s = new Scheme();
    assert_eval(s, "-3" , "-3");
    assert_eval(s, "-3.0" , "-3");
    assert_eval(s, "+3" , "3");
    assert_eval(s, "(- 3)" , "-3");
    assert_eval(s, "(- 3 2)" , "1");
    assert_eval(s, "(- 3 4 5)" , "-6");
    assert_eval(s, "(/ 2)" , "0.5");
    assert_eval(s, "(/ 10 2)" , "5");
    assert_eval(s, "(/ 10 2 2)" , "2.5");
    assert_eval(s, "(- 3 4 5)" , "-6");
    assert_eval(s, "(min 5)" , "5");
    assert_eval(s, "(min 3.0 1 2)" , "1");
    assert_eval(s, "(max 5)" , "5");
    assert_eval(s, "(max 3.0 1 2)" , "3");
    assert_eval(s, "(expt 3 4)" , "81");
    assert_eval(s, "(expt 0 0)" , "1");
    assert_eval(s, "(expt 0 3)" , "0");
    assert_eval(s, "(< 1 2 3)" , "#t");
    assert_eval(s, "(< 1 2 2 3)" , "#f");
    assert_eval(s, "(<= 1 2 2 3)" , "#t");
    assert_eval(s, "(<= 3 2 2 1)" , "#f");
    assert_eval(s, "(> 3 2 1)" , "#t");
    assert_eval(s, "(> 3 2 2 1)" , "#f");
    assert_eval(s, "(>= 3 2 2 1)" , "#t");
    assert_eval(s, "(>= 1 2 2 3)" , "#f");
    assert_eval(s, "(= 2 2 2 3)" , "#f");
    assert_eval(s, "(= 2 2 2 2)" , "#t");
    assert_eval(s, "(even? 10)" , "#t");
    assert_eval(s, "(even? -9)" , "#f");
    assert_eval(s, "(even? 0)" , "#t");
    assert_eval(s, "(odd? 31137)" , "#t");
    assert_eval(s, "(odd? 0)" , "#f");
    assert_eval(s, "(odd? -1)" , "#t");
    assert_eval(s, "(zero? 0)" , "#t");
    assert_eval(s, "(zero? -1)" , "#f");
    assert_eval(s, "(negative? 0)" , "#f");
    assert_eval(s, "(negative? -10)" , "#t");
    assert_eval(s, "(negative? 2)" , "#f");
    assert_eval(s, "(positive? 0)" , "#f");
    assert_eval(s, "(positive? -10)" , "#f");
    assert_eval(s, "(positive? 2)" , "#t");
    assert_eval(s, "(integer? 2)" , "#t");
    assert_eval(s, "(integer? 2.1)" , "#f");
    assert_eval(s, "(integer? 2.0)" , "#t");
    assert_eval(s, "(exact? 2.1)" , "#f");
    assert_eval(s, "(exact? 2)" , "#t");
    assert_eval(s, "(inexact? 2.1)" , "#t");
    assert_eval(s, "(inexact? 2)" , "#f");
    assert_eval(s, "(complex? 2)" , "#t");
    assert_eval(s, "(real? 2)" , "#t");
    assert_eval(s, "(rational? 2)" , "#t");

    assert_eval(s, "(round 2.1)" , "2");
    assert_eval(s, "(round 2.8)" , "3");
    assert_eval(s, "(floor -4.3)" , "-5");
    assert_eval(s, "(ceiling -4.3)" , "-4");
    assert_eval(s, "(truncate -4.3)" , "-4");
    assert_eval(s, "(round -4.3)" , "-4");
    assert_eval(s, "(floor 3.5)" , "3");
    assert_eval(s, "(ceiling 3.5)" , "4");
    assert_eval(s, "(truncate 3.5)" , "3");
    assert_eval(s, "(round 3.5)" , "4");
    assert_eval(s, "(round 2.5)" , "2"); // Round to nearest even integer
    assert_eval(s, "(round 7)" , "7");

    assert_eval(s, "(modulo 13 4)" , "1");
    assert_eval(s, "(remainder 13 4)" , "1");
    assert_eval(s, "(quotient 13 4)" , "3");
    assert_eval(s, "(modulo -13 4)" , "3");
    assert_eval(s, "(remainder -13 4)" , "-1");
    assert_eval(s, "(quotient -13 4)" , "-3");
    assert_eval(s, "(modulo 13 -4)" , "-3");
    assert_eval(s, "(remainder 13 -4)" , "1");
    assert_eval(s, "(quotient 13 -4)" , "-3");
    assert_eval(s, "(modulo -13 -4)" , "-1");
    assert_eval(s, "(remainder -13 -4)" , "-1");    
    assert_eval(s, "(quotient -13 -4)" , "3");
    assert_eval(s, "(gcd)" , "0");
    assert_eval(s, "(gcd 5)" , "5");
    assert_eval(s, "(gcd -4)" , "4"); // Guile 1.8.1 gets this one wrong.
    assert_eval(s, "(gcd -4 0)" , "4");
    assert_eval(s, "(gcd 0 4)" , "4");
    assert_eval(s, "(gcd 0 -4)" , "4");
    assert_eval(s, "(gcd 32 -36)" , "4");
    assert_eval(s, "(gcd 32 36 4 4 12)" , "4");
    assert_fail(s, "(gcd 'a)");  // Guile 1.8.1 gets this one wrong.
    assert_fail(s, "(gcd 1.1)");
    assert_eval(s, "(lcm)" , "1");
    assert_eval(s, "(lcm 0 0)" , "0");
    assert_eval(s, "(lcm 32 -36)" , "288");
    assert_eval(s, "(lcm 10 15 4)" , "60");
    assert_eval(s, "(lcm 10 15 -4)" , "60");
    assert_fail(s, "(lcm 'a)");
    assert_fail(s, "(lcm 1.1)");
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

    assert_eval(s, "(equal? #f '())", "#f");
    assert_eval(s, "(eqv? #f '())", "#f");
    assert_eval(s, "(eq? #f '())", "#f");
    assert_eval(s, "(eqv? #\\a #\\a)", "#t");
    assert_eval(s, "(eqv? #\\space #\\spAce)", "#t");
}

void test_pairs_and_lists() {
    SchemeObject* p = s_cons(SchemeObject::createSymbol("x"),SchemeObject::createSymbol("y"));
    assert(p->toString() == "(x . y)");
    
    Scheme* s = new Scheme();
    assert_eval(s, "(list? '())", "#t");
    assert_eval(s, "(list? '(1 2 3))", "#t");
    assert_eval(s, "(list? 1)", "#f");
    assert_eval(s, "(list? '(1 2 . 3))", "#f");
    
    // From R^5RS 6.3.2. Tests that list? returns #f on circular lists
    assert_eval(s, "(let ((x (list 'a))) (set-cdr! x x) (list? x))", "#f");
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
    assert_eval(s, "(cadr '((a b) (c d)))", "(c d)");
    assert_eval(s, "(cdar '((a b) (c d)))", "(b)");
    assert_eval(s, "(caadr '((a b) (c d)))", "c");

    assert_eval(s, "(reverse '(a b c))", "(c b a)");
    assert_eval(s, "(reverse '(a (b c) d (e (f))))","((e (f)) d (b c) a)");
    assert_eval(s, "(reverse '())", "()");
    assert_fail(s, "(reverse '(a b c d . e))");
    assert_fail(s, "(reverse)");
    assert_fail(s, "(reverse 'a)");
    assert_fail(s, "(reverse 'a 'b)");
    
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
    assert_eval(s, "(append '() '() 'b)", "b");
    assert_eval(s, "(append '() '() '())", "()");
    assert_eval(s, "(append 'a)", "a");
    assert_eval(s, "(append '(a b c) 'e)", "(a b c . e)");
    assert_eval(s, "(append '(a b c) '(1 . 2))", "(a b c 1 . 2)");
    assert_eval(s, "(append '(a (b)) '((c)))", "(a (b) (c))");
    assert_fail(s, "(append '(a . b) '(a b c))");
    assert_fail(s, "(append 'a '(a b c))");

    s->eval("(define e '((a 1) (b 2) (c 3)))");
    assert_eval(s, "(assq 'a e)", "(a 1)");
    assert_eval(s, "(assq 'b e)", "(b 2)");
    assert_eval(s, "(assq 'd e)", "#f");
    assert_eval(s, "(assoc (list 'a) '(((a)) ((b)) ((c))))", "((a))");
    assert_eval(s, "(assq (list 'a) '(((a)) ((b)) ((c))))", "#f");
    assert_eval(s, "(assv 5 '((2 3) (5 7) (11 13)))", "(5 7)");
    
    s->eval("(define e (list 'a 'b 'c 'd))");
    assert_eval(s, "(set-car! e 'f) e", "(f b c d)");
    assert_eval(s, "(set-cdr! e 'g) e", "(f . g)");
    assert_eval(s, "(set-cdr! e '()) e", "(f)");
    s->eval("(define (f) (list 'not-a-constant-list))");
    s->eval("(define (g) '(constant-list a b c))");
    assert_eval(s, "(set-car! (f) 3)", "#<unspecified>");
    assert_fail(s, "(set-car! (g) 3)");
    delete s;
}

void test_lambda() {
    Scheme* s = new Scheme();
    assert_eval(s, "(procedure? (lambda (x) x))", "#t");
    assert_eval(s, "(procedure? cons)", "#t");
    assert_eval(s, "(procedure? if)", "#t");
    assert_eval(s, "(procedure? 1)", "#f");
    assert_eval(s, "(procedure? 'car)","#f");
    assert_eval(s, "(procedure? (lambda (x) (* x x)))","#t");
    assert_eval(s, "(procedure? '(lambda (x) (* x x)))","#f");
    assert_eval(s, "((lambda () 3))", "3");
    assert_eval(s, "((lambda (x) (* 2 x)) 10)", "20");
    assert_eval(s, "((lambda (x y) (+  y x)) 7 10)", "17");
    assert_eval(s, "((lambda (x y z) (list z y x)) 3 4 5)", "(5 4 3)");
    // Some examples from R^5RS
    assert_eval(s, "((lambda x x) 3 4 5 6)", "(3 4 5 6)");
    assert_eval(s, "((lambda (x y . z) z) 3 4 5 6)", "(5 6)");
    assert_eval(s, "((lambda (x) (+ x x)) 4)", "8");
    assert_eval(s, "(define add4 (let ((x 4)) (lambda (y) (+ x y)))) (add4 6)", "10");
    
    assert_eval(s, "(let () (define (f . x) x) (f))", "()");
    assert_eval(s, "(let () (define (f . x) x) (f 1 2 3 4 5 6 7))", "(1 2 3 4 5 6 7)");
    assert_eval(s, "(let () (define f (lambda x x)) (f))", "()");
    assert_eval(s, "", "");
    assert_eval(s, "", "");
    assert_eval(s, "", "");
}

void test_macros() {
    Scheme* s = new Scheme();
    s->eval("(define-macro (greater-than x y) `(> ,x ,y))");
    assert_eval(s, "(greater-than 10 20)", "#f");
    assert_eval(s, "(greater-than 20 10)", "#t");

    s->eval("(define-macro (when test . consequent) `(if ,test (begin ,@consequent)))");
    assert_eval(s, "(when #t 'kaj)", "kaj");
}

void test_define_and_set() {
    Scheme* s = new Scheme();
    s->eval("(define x 17)");
    assert_eval(s, "x", "17");
    s->eval("(set! x 20)");
    assert_eval(s, "x", "20");
    s->eval("(let () (set! x 50))");
    assert_eval(s, "x", "50");
    s->eval("(define (square x) (* x x))");
    assert_eval(s, "(square 9)", "81");
    s->eval("(define (selftest . x) x)");
    // A R^5RS spec that guile 1.6.8 fails but we don't... Fixed in guile 1.8.1
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
    assert_eval(s, "(make-string 3)","\"   \"");
    assert_eval(s, "(make-string 5 #\\z)","\"zzzzz\"");
    assert_eval(s, "(string-length \"abcdef\")","6");
    assert_eval(s, "(string-length \"\")","0");
    assert_eval(s, "(string-length (make-string 200))","200");
    assert_eval(s, "(string-ref \"scheme\" 0)","#\\s");
    assert_eval(s, "(string-ref \"scheme\" 2)","#\\h");
    assert_eval(s, "(symbol->string 'aaa)","\"aaa\"");
    assert_eval(s, "(string->symbol \"aaa\")","aaa");
    assert_eval(s, "(eq? (string->symbol \"f\") (string->symbol \"F\"))", "#f");
    assert_eval(s, "(string-append)","\"\"");
    assert_eval(s, "(string-append \"zzz\")","\"zzz\"");
    assert_eval(s, "(string-append \"zzz\" \"xxx\") ","\"zzzxxx\"");
    assert_eval(s, "(string-copy \"zzz\")","\"zzz\"");
    assert_eval(s, "(define z \"zzz\") (define x (string-copy z)) (string-set! x 0 #\\x) z","\"zzz\"");
    assert_eval(s, "(string->number \"100\")", "100");
    assert_eval(s, "(string->number \"2.5\")", "2.5");
    assert_eval(s, "(string->number \"100\" 8)", "64");
    assert_eval(s, "(string->number \"\")", "#f");
    assert_eval(s, "(string->number \"+\")", "#f");
    assert_eval(s, "(string->number \"-\")", "#f");
    assert_eval(s, "(string->number \"d\")", "#f");
    assert_eval(s, "(string->number \"i\")", "#f");
    assert_eval(s, "(string->number \"I\")", "#f");
    assert_eval(s, "(string->number \"3i\")", "#f");
    assert_eval(s, "(string->number \"3.3i\")", "#f");
    assert_eval(s, "(string->number \".\")", "#f");
    assert_eval(s, "(string->number \"d\")", "#f");
    assert_eval(s, "(number->string 256)", "\"256\"");
    assert_eval(s, "(number->string 256 16)", "\"100\"");
    assert_eval(s, "(string->list \"\")", "()");
    assert_eval(s, "(string->list \"String\")", "(#\\S #\\t #\\r #\\i #\\n #\\g)");
    assert_eval(s, "(list->string '())", "\"\"");
    assert_eval(s, "(list->string '(#\\S #\\t #\\r #\\i #\\n #\\g))", "\"String\"");
    assert_eval(s, "(define ss (string #\\S #\\t #\\r #\\i #\\n #\\g)) ss", "\"String\"");
    assert_eval(s, "(string-set! ss 3 #\\u) ss", "\"Strung\"");
    assert_fail(s, "(string-set! ss 10 #\\u) ss");
    assert_fail(s, "(define (g) \"***\") (string-set! (g) 0 #\?)");
    assert_fail(s, "(string-set! (symbol->string 'immutable) 0 #\?)");
    assert_eval(s, "(substring \"ab\" 0 0)", "\"\"");
    assert_eval(s, "(substring \"ab\" 1 1)", "\"\"");
    assert_eval(s, "(substring \"ab\" 2 2)", "\"\"");
    assert_eval(s, "(substring \"ab\" 1 2)", "\"b\"");
    assert_eval(s, "(substring \"ab\" 0 1)", "\"a\"");
    assert_eval(s, "(substring \"ab\" 0 2)", "\"ab\"");
    assert_fail(s, "(substring \"abcdef\" 3 1)");
}

void test_begin() {
    Scheme* s = new Scheme();
    assert_eval(s, "(begin 1)", "1");
    assert_eval(s, "(begin 1 2)", "2");
    assert_eval(s, "(begin 1 2 3)", "3");
    assert_eval(s, "(begin)", "#<unspecified>");
}

void test_let() {
    Scheme* s = new Scheme();
    assert_eval(s, "(let () 'a 'b)", "b");
    assert_eval(s, "(let ((i 10)) i)", "10");
    assert_eval(s, "(let ((i 10)(j 20)) (* j i))", "200");
    assert_eval(s, "(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))", "70");
    assert_eval(s, "(let ((x 0)) (let ((x 1) (y (* x 1))) y))", "0");

    assert_eval(s, "(letrec ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1))))) (odd? (lambda (n) (if (zero? n) #f (even? (- n 1)))))) (even? 88))", "#t");

    assert_fail(s, "(let)");
    assert_fail(s, "(let 'a)");
    assert_fail(s, "(let 'a 'b)");
    assert_fail(s, "(let loop 'b)");
    assert_fail(s, "(let loop 'b 'c)");
    assert_fail(s, "(let*)");
    assert_fail(s, "(let* 'a)");
    assert_fail(s, "(let* 'a 'b)");
    assert_fail(s, "(letrec)");
    assert_fail(s, "(letrec 'a)");
    assert_fail(s, "(letrec 'a 'b)");


    // From http://sisc-scheme.org/r5rs_pitfall.scm
    assert_eval(s, "(let ((ls (list 1 2 3 4))) (append ls ls '(5)))", "(1 2 3 4 1 2 3 4 5)");
    assert_eval(s, "(let ((f -)) (let f ((n (f 1))) n))", "-1");
    assert_eval(s, "(let ((f -)) ((letrec ((f (lambda (n) n))) f) (f 1)))", "-1");

    assert_eval(s, "(let - ((n (- 1))) n)", "-1");

    assert_eval(s, "(let loop ((i 10)) i)", "10");
    assert_eval(s, "(let loop ((i 10)(j '())) (if (= 0 i) j (loop (- i 1) (cons i j))))", "(1 2 3 4 5 6 7 8 9 10)");
}

void test_do() {
    Scheme* s = new Scheme();
    assert_eval(s, "(do () (#t 'b))", "b");
    assert_eval(s, "(do () (#t))", "#<unspecified>");
    assert_fail(s, "(do)");
    assert_fail(s, "(do 'a)");
    assert_fail(s, "(do () ())");
    assert_fail(s, "(do (a) (#t))");
    assert_fail(s, "(do ((a 1) . (b 1)) (#t))");

    // From R^5RS 4.2.4 
    assert_eval(s, "(do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec) (vector-set! vec i i))", "#(0 1 2 3 4)");
    assert_eval(s, "(let ((x '(1 3 5 7 9))) (do ((x x (cdr x)) (sum 0 (+ sum (car x)))) ((null? x) sum)))", "25");
}

void test_quote() {
    Scheme* s = new Scheme();
    assert_eval(s, "'()", "()");
    assert_eval(s, "'(a b c)", "(a b c)");
    assert_eval(s, "'a", "a");
    assert_eval(s, "'1", "1");
    assert_eval(s, "(number? 1)", "#t");
    assert_eval(s, "(boolean? '#t)", "#t");

    assert_eval(s, "`a", "a");
    assert_eval(s, "`()", "()");
    assert_eval(s, "`1", "1");
    assert_eval(s, "`(a b c)", "(a b c)");
    assert_eval(s, "`(a b . c)", "(a b . c)");
    assert_eval(s, "`(a (+ 1 2) c)", "(a (+ 1 2) c)");
    assert_eval(s, "`(a b . ,(+ 1 2))", "(a b . 3)");
    assert_eval(s, "`(a ,(+ 1 2) c)", "(a 3 c)");
    assert_eval(s, "`(a ,1)", "(a 1)");
    assert_eval(s, "`(a `(b `c))", "(a (quasiquote (b (quasiquote c))))");
    assert_eval(s, "`,(+ 2 3)", "5");
    assert_eval(s, "(quasiquote (unquote (+ 2 3)))", "5");
    assert_eval(s, "`(a `(b ,(+ 1 2)))", "(a (quasiquote (b (unquote (+ 1 2)))))");
    assert_eval(s, "`(a ,(list 1 2 ) c)", "(a (1 2) c)");
    assert_eval(s, "`(a ,@(list 1 2 ) c)", "(a 1 2 c)");
    // From R^5RS 4.2.6
    assert_eval(s, "`(list ,(+ 1 2) 4)", "(list 3 4)");
    assert_eval(s, "(let ((name 'a)) `(list ,name ',name))", "(list a (quote a))");
    assert_eval(s, "`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)", "(a 3 4 5 6 b)");
    assert_eval(s, "`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))", "((foo 7) . cons)");
    assert_eval(s, "`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)", "#(10 5 2 4 3 8)");
    assert_eval(s, "`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)", "(a (quasiquote (b (unquote (+ 1 2)) (unquote (foo 4 d)) e)) f)");
    assert_eval(s, "(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))","(a (quasiquote (b (unquote x) (unquote (quote y)) d)) e)");
    assert_eval(s, "(quasiquote (list (unquote (+ 1 2)) 4))", "(list 3 4)");
    assert_eval(s, "'(quasiquote (list (unquote (+ 1 2)) 4))", "(quasiquote (list (unquote (+ 1 2)) 4))");
}

void test_map() {
    Scheme* s = new Scheme();
    assert_eval(s, "(map + '(1 2 3) '(10 20 30))", "(11 22 33)");
    assert_eval(s, "(map car '((a b) (d e) (g h)))", "(a d g)");
    assert_eval(s, "(map (lambda (n) (expt n n)) '(1 2 3 4 5))", "(1 4 27 256 3125)");
    assert_eval(s, "(let ((count 0)) (map (lambda (ignored) (set! count (+ count 1)) count) '(a b)))", "(1 2)");

    assert_fail(s, "(map)");
    assert_fail(s, "(map +)");
    assert_fail(s, "(map + '(1 2 3) '(1 2))");
    assert_fail(s, "(map + '(1 2) '(1 2 3))");
    assert_fail(s, "(map + '(1 2) 'a)");
    assert_fail(s, "(map + 'a '(1 2))");
}

void test_vector() {
    Scheme* s = new Scheme();
    SchemeObject* v = s->eval("(make-vector 3)");
    assert(v->getVectorElem(0) == S_UNSPECIFIED);
    assert(v->getVectorElem(1) == S_UNSPECIFIED);
    assert(v->getVectorElem(2) == S_UNSPECIFIED);
    assert_eval(s, "(make-vector 5 'a)", "#(a a a a a)");
    assert_eval(s, "(make-vector 2 (+ 5 1))", "#(6 6)");
    assert_eval(s, "(make-vector 0 'a)", "#()");
    assert_fail(s, "(make-vector -2 'a)");
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
    assert_fail(s, "(vector-length 'a)");
    assert_eval(s, "(list->vector '(a b c))", "#(a b c)");
    assert_eval(s, "(list->vector '())", "#()");
    assert_fail(s, "(list->vector 1)");
    assert_eval(s, "(vector->list #(a b c))","(a b c)");
    assert_eval(s, "(vector-ref #(a b c) 0)","a");
    assert_eval(s, "(vector-ref #(a b c) 1)","b");
    assert_eval(s, "(vector-ref #(a b c) 2)","c");
    assert_fail(s, "(vector-ref #(a b c) 3)");
    assert_fail(s, "(vector-ref #(a b c) -1)");
    assert_fail(s, "(vector-set! '#(0 1 2) 1 \"doe\")");   // Modifying immutable vector
    assert_eval(s, "(define v (vector 'a 'b 'c))(vector-set! v 1 '(1 2 3))v","#(a (1 2 3) c)");
    assert_eval(s, "(define z (make-vector 4))(vector-fill! z 'a)z","#(a a a a)");
    assert_fail(s, "(vector-set! (make-vector 5) -1 'a)");
    assert_fail(s, "(vector-set! (make-vector 5) 5 'a)");
}

void test_io() {
    Scheme* s = new Scheme();
    assert_eval(s, "(input-port? (current-input-port))","#t");
    assert_eval(s, "(output-port? (current-input-port))","#f");
    assert_eval(s, "(input-port? (current-output-port))","#f");
    assert_eval(s, "(output-port? (current-output-port))","#t");
}

void test_call_cc() {
    Scheme* s = new Scheme();
    assert_eval(s, "(call-with-current-continuation procedure?)", "#t");
    assert_eval(s, "(call/cc (lambda (exit) (for-each (lambda (x) (if (negative? x) (exit x))) '(54 0 37 -3 245 19)) #t))", "-3");
}

void test_eval() {
    Scheme* s = new Scheme();
    assert_eval(s, "(eval '(* 7 3) (scheme-report-environment 5))", "21");
    assert_eval(s, "(eval '(if #t 1 2) (null-environment 5))", "1");
    assert_eval(s, "(let ((f (eval '(lambda (f x) (f x x)) (null-environment 5)))) (f + 10))", "20");
}

int main(int argc, char *argv[]) {
    try {
        cout << "Test tokenizer...       ";
        test_tokenizer();
        cout << " OK" << endl;

        cout << "Test objects...         ";
        test_objects();
        cout << " OK" << endl;
        
        cout << "Test parser...          ";
        test_parser();
        cout << " OK" << endl;
        
        cout << "Test interpreter...     ";
        test_interpreter();
        cout << " OK" << endl;

        cout << "Test bools...           ";
        test_bools();
        cout << " OK" << endl;
        
        cout << "Test symbols...         ";
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

        cout << "Test quote...           ";
        test_quote();
        cout << " OK" << endl;

        cout << "Test lambda...          ";
        test_lambda();
        cout << " OK" << endl;

        cout << "Test macros...          ";
        test_macros();
        cout << " OK" << endl;

        cout << "Test let...             ";
        test_let();
        cout << " OK" << endl;

        cout << "Test do...              ";
        test_do();
        cout << " OK" << endl;

        cout << "Test char...            ";
        test_char();
        cout << " OK" << endl;

        cout << "Test string...          ";
        test_string();
        cout << " OK" << endl;

        cout << "Test map...             ";
        test_map();
        cout << " OK" << endl;

        cout << "Test call/cc...         ";
        test_call_cc();
        cout << " OK" << endl;

        cout << "Test eval...            ";
        test_eval();
        cout << " OK" << endl;

        cout << "Test I/O...             ";
        test_io();
        cout << " OK" << endl;

        cout << "Test define and set...  ";
        test_define_and_set();
        cout << " OK" << endl;

        test_begin();

    } catch (scheme_exception e) {
		cerr << "Exception: " << e.str << endl;
        return EXIT_FAILURE;
    }
    
    return errors_found == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
