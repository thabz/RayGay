
#include "r6rs-lib-lists.h"

Scheme* thescheme;

SchemeObject* s_cons_star(int num, SchemeStack::iterator stack) {
    assert(num > 0);        
    if (num == 1) {
        return *stack;
    }
    SchemeObject* result;
    for(int i = 0; i < num-1; i++) {
  
    }
}

SchemeObject* s_filter(SchemeObject* proc, SchemeObject* list) {
    vector<SchemeObject*> vec;
    while(list != S_EMPTY_LIST) {
	if (thescheme->callProcedure_1(proc, i_car(list)) != S_FALSE)  {
	    vec.push_back(i_car(list));
	}
    }
    SchemeObject* r = S_EMPTY_LIST;
    for(uint32_t i = 0; i < vec.size(); i++) {
	r = i_cons(vec[i],r);
    }
    return r;
}

SchemeObject* s_partition(SchemeObject* proc, SchemeObject* list) {
    vector<SchemeObject*> true_vec;
    vector<SchemeObject*> false_vec;
    while(list != S_EMPTY_LIST) {
	if (thescheme->callProcedure_1(proc, i_car(list)) != S_FALSE)  {
	    false_vec.push_back(i_car(list));
	} else {
	    false_vec.push_back(i_car(list));
	}
    }
    SchemeObject* true_list = S_EMPTY_LIST;
    SchemeObject* false_list = S_EMPTY_LIST;
    for(uint32_t i = 0; i < true_vec.size(); i++) {
	true_list = i_cons(true_vec[i],true_list);
    }
    for(uint32_t i = 0; i < false_vec.size(); i++) {
	false_list = i_cons(false_vec[i],false_list);
    }
    // TODO: Return two values
    return i_cons(true_list,false_list);
}

void R6RSLibLists::bind(Scheme* scheme, SchemeObject* envt) {
    thescheme = scheme;
    scheme->assign(L"cons*"                     ,1,0,1, (SchemeObject* (*)()) s_cons_star, envt);
    scheme->assign(L"filter"                    ,2,0,0, (SchemeObject* (*)()) s_filter, envt);
    scheme->assign(L"partition"                 ,2,0,0, (SchemeObject* (*)()) s_partition, envt);
}

