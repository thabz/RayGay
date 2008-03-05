
#include "r6rs-lib-lists.h"

SchemeObject* s_find(Scheme* scheme, SchemeObject* proc, SchemeObject* list) {
    while(list != S_EMPTY_LIST) {
	if (scheme->callProcedure_1(proc, i_car(list)) != S_FALSE)  {
	    return i_car(list);
	}
	list = i_cdr(list);
    }
    return S_FALSE;
}

// TODO: Implement this as a macro. The last call should be tail-recursive
// according to the spec.
SchemeObject* s_for_all(Scheme* scheme, int num, SchemeStack::iterator args) {
    wstring procname = L"for-all";
    SchemeObject* proc = args[0];
    assert_arg_procedure_type(procname, 1, proc);

    int empty = 0;
    SchemeObject* cropped_args[num-1];
    for(int i = 1; i < num; i++) {
        assert_non_atom_type(procname, i, args[i]);
        cropped_args[i-1] = args[i];
	    if (args[i] == S_EMPTY_LIST) empty++;
    }
    if (empty == num-1) return S_TRUE;

    // Vi skralder af lists i hvert gennemløb. Så ((1 2 3)(10 20 30)) bliver 
    // til ((2 3)(20 30)) og til sidst ((3)(30))
    while (i_cdr(cropped_args[0]) != S_EMPTY_LIST) {
        SchemeAppendableList collection;
        for(int i = 0; i < num-1; i++) {
            SchemeObject* lists_ptr = cropped_args[i];
            if (lists_ptr == S_EMPTY_LIST) {
                throw scheme_exception(procname + L": argument lists not equals length.");
            }
            SchemeObject* arg = s_car(scheme,lists_ptr);
            cropped_args[i] = s_cdr(scheme,lists_ptr);
            collection.add(arg);
	    }

	    SchemeObject* result_item = scheme->callProcedure_n(proc, collection.list);
	    if (result_item == S_FALSE) {
	        return S_FALSE;
	    }
    }

    // Tjek at alle cropped_args kun indeholder et element, dvs. at 
    // argumentlisterne var lige lange
    SchemeAppendableList collection;
    for(int i = 0; i < num-1; i++) {
        if (i_cdr(cropped_args[i]) != S_EMPTY_LIST) {
            throw scheme_exception(procname + L": argument lists not equals length.");
        }
	    collection.add(i_car(cropped_args[i]));
    }
    return scheme->callProcedure_n(proc, collection.list);
}


// TODO: Implement this as a macro. The last call should be tail-recursive
// according to the spec.
SchemeObject* s_exists(Scheme* scheme, int num, SchemeStack::iterator args) {
    wstring procname = L"exists";
    SchemeObject* proc = args[0];
    assert_arg_procedure_type(procname, 1, proc);

    int empty = 0;
    SchemeObject* cropped_args[num-1];
    for(int i = 1; i < num; i++) {
        assert_non_atom_type(procname, i, args[i]);
        cropped_args[i-1] = args[i];
	if (args[i] == S_EMPTY_LIST) empty++;
    }
    if (empty == num-1) return S_FALSE;

    // Vi skralder af lists i hvert gennemløb. Så ((1 2 3)(10 20 30)) bliver 
    // til ((2 3)(20 30)) og til sidst ((3)(30))
    while (i_cdr(cropped_args[0]) != S_EMPTY_LIST) {
        SchemeAppendableList collection;
        for(int i = 0; i < num-1; i++) {
            SchemeObject* lists_ptr = cropped_args[i];
            if (lists_ptr == S_EMPTY_LIST) {
                throw scheme_exception(procname + L": argument lists not equals length.");
            }
            SchemeObject* arg = s_car(scheme,lists_ptr);
            cropped_args[i] = s_cdr(scheme,lists_ptr);
            collection.add(arg);
	    }

	    SchemeObject* result_item = scheme->callProcedure_n(proc, collection.list);
	    if (result_item != S_FALSE) {
	        return result_item;
	    }
    }

    // Tjek at alle cropped_args kun indeholder et element, dvs. at 
    // argumentlisterne var lige lange
    SchemeAppendableList collection;
    for(int i = 0; i < num-1; i++) {
        if (i_cdr(cropped_args[i]) != S_EMPTY_LIST) {
            throw scheme_exception(procname + L": argument lists not equals length.");
        }
	collection.add(i_car(cropped_args[i]));
    }
    return scheme->callProcedure_n(proc, collection.list);
}

SchemeObject* s_filter(Scheme* scheme, SchemeObject* proc, SchemeObject* list) {
    vector<SchemeObject*> vec;
    while(list != S_EMPTY_LIST) {
	if (scheme->callProcedure_1(proc, i_car(list)) != S_FALSE)  {
	    vec.push_back(i_car(list));
	}
	list = i_cdr(list);
    }
    SchemeObject* r = S_EMPTY_LIST;
    for(uint32_t i = 0; i < vec.size(); i++) {
	r = i_cons(vec[vec.size()-1-i],r);
    }
    return r;
}

SchemeObject* s_partition(Scheme* scheme, SchemeObject* proc, SchemeObject* list) {
    vector<SchemeObject*> true_vec;
    vector<SchemeObject*> false_vec;
    while(list != S_EMPTY_LIST) {
	if (scheme->callProcedure_1(proc, i_car(list)) != S_FALSE)  {
	    true_vec.push_back(i_car(list));
	} else {
	    false_vec.push_back(i_car(list));
	}
	list = i_cdr(list);
    }
    SchemeObject* true_list = S_EMPTY_LIST;
    SchemeObject* false_list = S_EMPTY_LIST;
    for(uint32_t i = 0; i < true_vec.size(); i++) {
	true_list = i_cons(true_vec[true_vec.size()-1-i],true_list);
    }
    for(uint32_t i = 0; i < false_vec.size(); i++) {
	false_list = i_cons(false_vec[false_vec.size()-1-i],false_list);
    }
    // Return two values
    return i_cons(true_list,i_cons(false_list,S_EMPTY_LIST));
}

SchemeObject* s_fold_left(Scheme* scheme, int num, SchemeStack::iterator args) {
    wstring procname = L"fold-left";
    SchemeObject* proc = args[0];
    assert_arg_procedure_type(procname, 1, proc);

    SchemeObject* nil = args[1];

    SchemeObject* cropped_args[num-2];
    for(int i = 2; i < num; i++) {
        assert_non_atom_type(procname, i, args[i]);
        cropped_args[i-2] = args[i];
    }

    // Vi skralder af lists i hvert gennemløb. Så ((1 2 3)(10 20 30)) bliver 
    // til ((2 3)(20 30)) og til sidst ((3)(30))
    while (cropped_args[0] != S_EMPTY_LIST) {
        SchemeAppendableList collection;
	    collection.add(nil);
        for(int i = 0; i < num-2; i++) {
            SchemeObject* lists_ptr = cropped_args[i];
            if (lists_ptr == S_EMPTY_LIST) {
                throw scheme_exception(procname + L": argument lists not equals length.");
            }
            SchemeObject* arg = s_car(scheme,lists_ptr);
            cropped_args[i] = s_cdr(scheme,lists_ptr);
            collection.add(arg);
	    }

	    nil = scheme->callProcedure_n(proc, collection.list);
    }

    // Tjek at alle cropped_args kun indeholder et element, dvs. at 
    // argumentlisterne var lige lange
    SchemeAppendableList collection;
    for(int i = 0; i < num-2; i++) {
        if (cropped_args[i] != S_EMPTY_LIST) {
            throw scheme_exception(procname + L": argument lists not equals length.");
        }
    }
    return nil;
}

SchemeObject* s_fold_right(Scheme* scheme, int num, SchemeStack::iterator args) {
    return S_FALSE;
}

SchemeObject* s_remp(Scheme* scheme, SchemeObject* proc, SchemeObject* list) {
    vector<SchemeObject*> vec;
    while(list != S_EMPTY_LIST) {
	if (scheme->callProcedure_1(proc, i_car(list)) == S_FALSE)  {
	    vec.push_back(i_car(list));
	}
	list = i_cdr(list);
    }
    SchemeObject* r = S_EMPTY_LIST;
    for(uint32_t i = 0; i < vec.size(); i++) {
	r = i_cons(vec[vec.size()-1-i],r);
    }
    return r;
}

SchemeObject* s_remove(Scheme* scheme, SchemeObject* obj, SchemeObject* list) {
    vector<SchemeObject*> vec;
    while(list != S_EMPTY_LIST) {
	if (s_equal_p(scheme,obj, i_car(list)) == S_FALSE)  {
	    vec.push_back(i_car(list));
	}
	list = i_cdr(list);
    }
    SchemeObject* r = S_EMPTY_LIST;
    for(uint32_t i = 0; i < vec.size(); i++) {
	r = i_cons(vec[vec.size()-1-i],r);
    }
    return r;
}

SchemeObject* s_remv(Scheme* scheme, SchemeObject* obj, SchemeObject* list) {
    vector<SchemeObject*> vec;
    while(list != S_EMPTY_LIST) {
	if (s_eqv_p(scheme,obj, i_car(list)) == S_FALSE)  {
	    vec.push_back(i_car(list));
	}
	list = i_cdr(list);
    }
    SchemeObject* r = S_EMPTY_LIST;
    for(uint32_t i = 0; i < vec.size(); i++) {
	r = i_cons(vec[vec.size()-1-i],r);
    }
    return r;
}

SchemeObject* s_remq(Scheme* scheme, SchemeObject* obj, SchemeObject* list) {
    vector<SchemeObject*> vec;
    while(list != S_EMPTY_LIST) {
	if (s_eq_p(scheme,obj, i_car(list)) == S_FALSE)  {
	    vec.push_back(i_car(list));
	}
	list = i_cdr(list);
    }
    SchemeObject* r = S_EMPTY_LIST;
    for(uint32_t i = 0; i < vec.size(); i++) {
	r = i_cons(vec[vec.size()-1-i],r);
    }
    return r;
}

inline
SchemeObject* member_helper(Scheme* scheme, SchemeObject* (comparator)(Scheme*,SchemeObject*,SchemeObject*), SchemeObject* obj, SchemeObject* p) {
    while (i_null_p(p) == S_FALSE) {
        if ((*comparator)(scheme, obj, s_car(scheme,p)) == S_TRUE) {
            return p;
        } else {
            p = s_cdr(scheme,p);
        }
    }
    return S_FALSE;
}

SchemeObject* s_member(Scheme* scheme, SchemeObject* obj, SchemeObject* p) {
    return member_helper(scheme, s_equal_p, obj, p);
}

SchemeObject* s_memq(Scheme* scheme, SchemeObject* obj, SchemeObject* p) {
    return member_helper(scheme, s_eq_p, obj, p);
}

SchemeObject* s_memv(Scheme* scheme, SchemeObject* obj, SchemeObject* p) {
    return member_helper(scheme, s_eqv_p, obj, p);
}

SchemeObject* s_memp(Scheme* scheme, SchemeObject* proc, SchemeObject* list) {
    while (list != S_EMPTY_LIST) {
	if (scheme->callProcedure_1(proc, i_car(list)) != S_FALSE)  {
	    return list;
	} else {
	    list = i_cdr(list);
	}
    }
    return S_FALSE;
}

SchemeObject* assoc_helper(Scheme* scheme, SchemeObject* (comparator)(Scheme*,SchemeObject*,SchemeObject*), SchemeObject* obj, SchemeObject* alist) {
    while (alist != S_EMPTY_LIST) {
        if (i_pair_p(s_car(scheme,alist)) == S_FALSE) {
            throw scheme_exception(L"Illegal argument");
        }
        SchemeObject* p = s_car(scheme, alist);
        if ((*comparator)(scheme, obj, s_car(scheme, p)) == S_TRUE) {
            return p;
        }
        alist = s_cdr(scheme,alist);
    }
    return S_FALSE;
}

SchemeObject* s_assoc(Scheme* scheme, SchemeObject* obj, SchemeObject* alist) {
    return assoc_helper(scheme, s_equal_p, obj, alist); 
}

SchemeObject* s_assq(Scheme* scheme, SchemeObject* obj, SchemeObject* alist) {
    return assoc_helper(scheme, s_eq_p, obj, alist); 
}

SchemeObject* s_assv(Scheme* scheme, SchemeObject* obj, SchemeObject* alist) {
    return assoc_helper(scheme, s_eqv_p, obj, alist); 
}

SchemeObject* s_assp(Scheme* scheme, SchemeObject* proc, SchemeObject* alist) {
    while (alist != S_EMPTY_LIST) {
        if (i_pair_p(s_car(scheme,alist)) == S_FALSE) {
            throw scheme_exception(L"Illegal argument");
        }
	if (scheme->callProcedure_1(proc, i_car(i_car(alist))) != S_FALSE)  {
	    return i_car(alist);
	} else {
	    alist = i_cdr(alist);
	}
    }
    return S_FALSE;
}

SchemeObject* s_cons_star(Scheme* scheme, int num, SchemeStack::iterator stack) {
    if (num == 1) {
        return *stack;
    }

    SchemeObject* result = i_cons(stack[num-2],stack[num-1]);

    for(int i = num-3; i >= 0; i--) {
       result = i_cons(stack[i], result);
    }

    return result;
}


void R6RSLibLists::bind(Scheme* scheme, SchemeObject* envt) {
    scheme->assign(L"find"            ,2,0,0, (SchemeObject* (*)()) s_find, envt);
    scheme->assign(L"for-all"         ,2,0,1, (SchemeObject* (*)()) s_for_all, envt);
    scheme->assign(L"exists"          ,2,0,1, (SchemeObject* (*)()) s_exists, envt);
    scheme->assign(L"filter"          ,2,0,0, (SchemeObject* (*)()) s_filter, envt);
    scheme->assign(L"partition"       ,2,0,0, (SchemeObject* (*)()) s_partition, envt);
    scheme->assign(L"fold-left"       ,3,0,1, (SchemeObject* (*)()) s_fold_left, envt);
    scheme->assign(L"fold-right"      ,3,0,1, (SchemeObject* (*)()) s_fold_right, envt);
    scheme->assign(L"remp"            ,2,0,0, (SchemeObject* (*)()) s_remp, envt);
    scheme->assign(L"remove"          ,2,0,0, (SchemeObject* (*)()) s_remove, envt);
    scheme->assign(L"remq"            ,2,0,0, (SchemeObject* (*)()) s_remq, envt);
    scheme->assign(L"remv"            ,2,0,0, (SchemeObject* (*)()) s_remv, envt);
    scheme->assign(L"memp"            ,2,0,0, (SchemeObject* (*)()) s_memp, envt);
    scheme->assign(L"member"          ,2,0,0, (SchemeObject* (*)()) s_member, envt);
    scheme->assign(L"memq"            ,2,0,0, (SchemeObject* (*)()) s_memq, envt);
    scheme->assign(L"memv"            ,2,0,0, (SchemeObject* (*)()) s_memv, envt);
    scheme->assign(L"assp"            ,2,0,0, (SchemeObject* (*)()) s_assp, envt);
    scheme->assign(L"assoc"           ,2,0,0, (SchemeObject* (*)()) s_assoc, envt);
    scheme->assign(L"assq"            ,2,0,0, (SchemeObject* (*)()) s_assq, envt);
    scheme->assign(L"assv"            ,2,0,0, (SchemeObject* (*)()) s_assv, envt);
    scheme->assign(L"cons*"           ,1,0,1, (SchemeObject* (*)()) s_cons_star, envt);
}

