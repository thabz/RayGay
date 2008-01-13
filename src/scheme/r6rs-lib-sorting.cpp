
#include "r6rs-lib-sorting.h"

// Skift til mergesort. Java og Perl bruger mergesort.
//
// As of Perl 5.8, merge sort is its default sorting algorithm 
// (it was quicksort in previous versions of Perl). In Java, the 
// Arrays.sort() methods use mergesort or a tuned quicksort depending 
// on the datatypes[1] and for implementation efficiency switch when 
// fewer than seven array elements are being sorted.[2]
//
// Quicksort er kun bedre, hvis du ved at du har et array af tal og 
// dermed kan finde den perfekte pivot-værdi i linær tid.

// FIXME: Make this class an object with it's very own
// pointer to a scheme instance instead of calling static bind.
Scheme* thescheme;

void merge(SchemeObject* proc,
   	   SchemeObject** list1, int size1, 
	   SchemeObject** list2, int size2,
	   SchemeObject** list3) {
    int i1 = 0, i2 = 0, i3 = 0;
    while(i1 < size1 && i2 < size2) {
	    if (thescheme->callProcedure_2(proc, list1[i1], list2[i2]) == S_TRUE) {
	        list3[i3++] = list1[i1++];
	    } else{
	        list3[i3++] = list2[i2++];
	    }
    }
    while(i1 < size1) {
	    list3[i3++] = list1[i1++];
    }
    while(i2 < size2) {
	    list3[i3++] = list2[i2++];
    }
}

void mergesort(SchemeObject* proc, SchemeObject** array, int32_t size, SchemeObject** tmp) {
    if (size <= 1) return;

    int mid = size / 2;
    mergesort(proc, array, mid, tmp);
    mergesort(proc, array+mid, size-mid, tmp+mid);
    merge(proc, array, mid, array+mid, size-mid, tmp);
    for(int i = 0; i < size; i++) {
	array[i] = tmp[i];
    }
}

SchemeObject* s_vector_sort_e(SchemeObject* proc, SchemeObject* vec) {
    assert_arg_procedure_that_take(L"vector-sort!", 1, proc, 2);
    SchemeObject* tmp[vec->length];
    mergesort(proc, vec->elems, vec->length, tmp);
    return S_UNSPECIFIED;
}

SchemeObject* s_list_sort(SchemeObject* proc, SchemeObject* list) {
    assert_arg_procedure_that_take(L"list-sort", 1, proc, 2);
    SchemeObject* vec = s_list_2_vector(list);
    vec->set_gc_protected(true);
    s_vector_sort_e(proc,vec);
    vec->set_gc_protected(false);
    SchemeObject* result = s_vector_2_list(vec);
    return result;
}

SchemeObject* s_vector_sort(SchemeObject* proc, SchemeObject* vec) {
    assert_arg_procedure_that_take(L"vector-sort", 1, proc, 2);
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
    thescheme = scheme;
    scheme->assign(L"list-sort"   ,2,0,0, (SchemeObject* (*)()) s_list_sort, envt);
    scheme->assign(L"vector-sort" ,2,0,0, (SchemeObject* (*)()) s_vector_sort, envt);
    scheme->assign(L"vector-sort!",2,0,0, (SchemeObject* (*)()) s_vector_sort_e, envt);
}

