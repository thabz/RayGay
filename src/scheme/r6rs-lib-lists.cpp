
#include "r6rs-lib-lists.h"

SchemeObject* s_cons_star(int num, SchemeStack::iterator stack) {
    assert(num > 0);        
    if (num == 1) {
        return *stack;
    }
    SchemeObject* result;
    for(int i = 0; i < num-1; i++) {
  
    }
    
}

void R6RSLibLists::bind(Scheme* scheme, SchemeObject* envt) {
    scheme->assign("cons*"                     ,1,0,1, (SchemeObject* (*)()) s_cons_star, envt);
}
