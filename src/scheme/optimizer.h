
#ifndef SCHEME_OPTIMIZER_H
#define SCHEME_OPTIMIZER_H

#include "scheme.h"

/**
 * The optimizer takes raw scheme sourcecode expressions and replaces them with new and faster expressions.
 * 
 * Optimization to help the interpreter are:
 * 1) Binding lookups are optimized with embedded stepping hints. (Eg. binding is located two environments up
 *    and is third pair in binding-list)
 * 2) Number of arguments are checked, so runtime don't have to.
 * 3) Do-exprs var-triples and split into the symbol, init and step lists to ease the work of the interpreter.
 * 
 * Math-optimizations:
 * 1) (* .... 0) are replaced with 0 if * is still bound to our internal plus-function at the time of evaluation.
 * 2) Constant folding: (+ 1 2 3 x) are replaced with (+ 6 x) if + is still bound to... etc.
 */
class Optimizer {
    public:
        static SchemeObject* optimizeList(SchemeObject* list, SchemeObject* envt);
        
    private:
        static SchemeObject* optimizeSymbol(SchemeObject* symbol, SchemeObject* envt);
        static SchemeObject* optimizeDo(SchemeObject* do_expr, SchemeObject* envt);
        static SchemeObject* optimizeIf(SchemeObject* do_expr, SchemeObject* envt);
        static SchemeObject* optimizeLet(SchemeObject* do_expr, SchemeObject* envt);
};

#endif