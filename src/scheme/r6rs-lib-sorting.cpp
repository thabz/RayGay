
#include "r6rs-lib-sorting.h"

SchemeObject* s_vector_sort_e(SchemeObject* proc, SchemeObject* vec) {
    return S_UNSPECIFIED;
}

SchemeObject* s_list_sort(SchemeObject* proc, SchemeObject* list) {
    SchemeObject* vec = s_list_2_vector(list);
    vec->set_gc_protected(true);
    s_vector_sort_e(proc,vec);
    vec->set_gc_protected(false);
    SchemeObject* result = s_vector_2_list(vec);
    return result;
}

SchemeObject* s_vector_sort(SchemeObject* proc, SchemeObject* vec) {
    SchemeObject* result = SchemeObject::createVector(S_UNSPECIFIED, vec->length);
    for(int32_t i = 0; i < vec->length; i++) {
	result->setVectorElem(vec->getVectorElem(i), i);
    }
    result->set_gc_protected(true);
    s_vector_sort_e(proc, result);
    result->set_gc_protected(false);
    return result;
}

void R6RSLibSorting::bind(Scheme* scheme, SchemeObject* envt) {
    scheme->assign("list-sort"   ,2,0,0, (SchemeObject* (*)()) s_list_sort, envt);
    scheme->assign("vector-sort" ,2,0,0, (SchemeObject* (*)()) s_vector_sort, envt);
    scheme->assign("vector-sort!",2,0,0, (SchemeObject* (*)()) s_vector_sort_e, envt);
}

