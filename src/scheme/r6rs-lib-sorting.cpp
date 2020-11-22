
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

void merge(Scheme *scheme, SchemeObject *proc, SchemeObject **list1, int size1,
           SchemeObject **list2, int size2, SchemeObject **list3) {
  int i1 = 0, i2 = 0, i3 = 0;
  while (i1 < size1 && i2 < size2) {
    if (scheme->callProcedure_2(proc, list1[i1], list2[i2]) != S_FALSE) {
      list3[i3++] = list1[i1++];
    } else {
      list3[i3++] = list2[i2++];
    }
  }
  while (i1 < size1) {
    list3[i3++] = list1[i1++];
  }
  while (i2 < size2) {
    list3[i3++] = list2[i2++];
  }
}

void mergesort(Scheme *scheme, SchemeObject *proc, SchemeObject **array,
               int32_t size, SchemeObject **tmp) {
  if (size <= 1)
    return;

  int mid = size / 2;
  mergesort(scheme, proc, array, mid, tmp);
  mergesort(scheme, proc, array + mid, size - mid, tmp + mid);
  merge(scheme, proc, array, mid, array + mid, size - mid, tmp);
  for (int i = 0; i < size; i++) {
    array[i] = tmp[i];
  }
}

SchemeObject *s_vector_sort_e(Scheme *scheme, SchemeObject *proc,
                              SchemeObject *vec) {
  //    assert_arg_procedure_that_take(L"vector-sort!", 1, proc, 2);
  SchemeObject *tmp[vec->length];
  mergesort(scheme, proc, vec->elems, vec->length, tmp);
  return S_UNSPECIFIED;
}

SchemeObject *s_list_sort(Scheme *scheme, SchemeObject *proc,
                          SchemeObject *list) {
  //    assert_arg_procedure_that_take(L"list-sort", 1, proc, 2);
  int64_t len = scm2int(s_length(scheme, list));
  SchemeObject *vec[len];
  SchemeObject *tmp[len];
  for (int64_t i = 0; i < len; i++) {
    vec[i] = i_car(list);
    list = i_cdr(list);
  }
  mergesort(scheme, proc, vec, len, tmp);
  SchemeObject *result = S_EMPTY_LIST;
  for (int64_t i = 0; i < len; i++) {
    result = i_cons(vec[len - 1 - i], result);
  }
  return result;
}

SchemeObject *s_vector_sort(Scheme *scheme, SchemeObject *proc,
                            SchemeObject *vec) {
  // TODO: The check below doesn't work for user-defined lambdas.
  //    assert_arg_procedure_that_take(L"vector-sort", 1, proc, 2);
  SchemeObject **vect = new SchemeObject *[vec->length];
  SchemeObject *tmp[vec->length];
  for (uint32_t i = 0; i < vec->length; i++) {
    vect[i] = vec->getVectorElem(i);
  }
  mergesort(scheme, proc, vect, vec->length, tmp);
  return SchemeObject::createVector(vect, vec->length);
}

void R6RSLibSorting::bind(Scheme *scheme, SchemeObject *envt) {
  scheme->assign(L"list-sort", 2, 0, 0, (SchemeObject * (*)()) s_list_sort,
                 envt);
  scheme->assign(L"vector-sort", 2, 0, 0, (SchemeObject * (*)()) s_vector_sort,
                 envt);
  scheme->assign(L"vector-sort!", 2, 0, 0,
                 (SchemeObject * (*)()) s_vector_sort_e, envt);
}
