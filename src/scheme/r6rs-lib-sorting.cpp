
#include "r6rs-lib-sorting.h"

void swap(SchemeObject* vec, int32_t i, int32_t j) {
    SchemeObject* tmp = vec->getVectorElem(i);
    vec->setVectorElem(vec->getVectorElem(j), i);
    vec->setVectorElem(tmp, j);            
}

void quicksort(SchemeObject* proc, SchemeObject* vec, int32_t l, int32_t u) {
    if (u <= 1) return;
    SchemeObject* t = vec->getVectorElem(0);
    int32_t i = l;
    int32_t j = u;

}

SchemeObject* s_vector_sort_e(SchemeObject* proc, SchemeObject* vec) {
    quicksort(proc, vec, 0, vec->length);        
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
    scheme->assign(L"list-sort"   ,2,0,0, (SchemeObject* (*)()) s_list_sort, envt);
    scheme->assign(L"vector-sort" ,2,0,0, (SchemeObject* (*)()) s_vector_sort, envt);
    scheme->assign(L"vector-sort!",2,0,0, (SchemeObject* (*)()) s_vector_sort_e, envt);
}

