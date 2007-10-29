
#include "optimizer.h"

/**
 * If the list-expression can be optimized this code returns a new list (++MEMOIZED++xx++ ...)
 * or the original list wrapped in (++NOTMEMOIZED++xx++ ...) if no optimizations could be applied.
 * We wrap unmodified lists too so that the caller continue to resubmit code we've already tried to
 * optimize.
 * 
 * The caller should then copy car and cdr of returned list into head-pair of original list
 * to modify the running source code in place.
 */ 
SchemeObject* Optimizer::optimizeList(SchemeObject* list, SchemeObject* envt) {
    // To write the modified list into program-source-memory just modify list's car and cdr
    // and it's a completely new list.        
    SchemeObject* car = i_car(list);
    if (i_symbol_p(car) == S_FALSE) {
        // We don't handle combinations ((x) y z) yet   
        return list;    
    }
    
    SchemeObject* proc = envt->getBinding(car);
    if (proc == NULL) {
        throw scheme_exception(L"Unbound variable: " + car->toString());
    }
    
    if (proc->type() != SchemeObject::USER_PROCEDURE) {
        return list;    
    }
    
    // Optimize the args
    SchemeObject* args = i_cdr(list);
    SchemeObject* optimized_args = S_EMPTY_LIST;
    while (args != S_EMPTY_LIST) {
        SchemeObject* arg = i_car(args);
        SchemeObject* optimized_arg;
        if (arg->type() == SchemeObject::SYMBOL) {
            optimized_arg = optimizeSymbol(arg, envt);
        } else if (arg->type() == SchemeObject::PAIR) {
            optimized_arg = optimizeList(arg, envt);
        } else {
            optimized_arg = arg;        
        }
        optimized_args = i_cons(optimized_arg, optimized_args);
        args = i_cdr(args);    
    }
    optimized_args = s_reverse(optimized_args);
    
    return list;
}

SchemeObject* Optimizer::optimizeSymbol(SchemeObject* symbol, SchemeObject* envt) {
    return symbol;
}