
#include "r6rs-lib-sorting.h"

SchemeObject* s_list_sort(SchemeObject* proc, SchemeObject* list) {
        
}

SchemeObject* s_vector_sort(SchemeObject* proc, SchemeObject* vec) {
        
}

SchemeObject* s_vector_sort_e(SchemeObject* proc, SchemeObject* vec) {
        
}

void R6RSLibSorting::bind(Scheme* scheme, SchemeObject* envt) {
    scheme->assign("list-sort"   ,2,0,0, (SchemeObject* (*)()) s_list_sort, envt);
    scheme->assign("vector-sort" ,2,0,0, (SchemeObject* (*)()) s_vector_sort, envt);
    scheme->assign("vector-sort!",2,0,0, (SchemeObject* (*)()) s_vector_sort_e, envt);
}
