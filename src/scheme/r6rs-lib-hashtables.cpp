
#include "r6rs-lib-hashtables.h"
#include <hash_set>

using namespace std;

Scheme* myscheme;

struct eqkey {
    eqkey::eqkey(SchemeObject* equiv) {
        this->equiv = equiv;
    }

    bool operator()(SchemeObject* o1, SchemeObject* o2) {
        return S_TRUE == myscheme->callProcedure_2(equiv, o1, o2);
    } 
    
    SchemeObject* equiv;
};


SchemeObject* s_make_eq_hashtable(SchemeObject* k) {
}

SchemeObject* s_make_eqv_hashtable(SchemeObject* k) {
}

SchemeObject* s_make_hashtable(SchemeObject* hash_func, SchemeObject* equiv, SchemeObject* k) {
    if (k == S_UNSPECIFIED) {
        size = 10;
    } else {
        int64_t size = scm2int(k);
     }
    
}

SchemeObject* s_hashtable_p(SchemeObject* o) {

}

void R6RSLibArithmetic::bind(Scheme* scheme, SchemeObject* envt) {
    myscheme = scheme;
    scheme->assign(L"make-eq-hashtable"    ,0,1,0, (SchemeObject* (*)()) s_make_eq_hashtable, envt);
    scheme->assign(L"make-eqv-hashtable"   ,0,1,0, (SchemeObject* (*)()) s_make_eqv_hashtable, envt);
    scheme->assign(L"make-hashtable"       ,2,1,0, (SchemeObject* (*)()) s_make_hashtable, envt);
    scheme->assign(L"hashtable?"           ,1,0,0, (SchemeObject* (*)()) s_hashtable_p, envt);
}
