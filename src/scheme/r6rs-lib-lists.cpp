
#include "r6rs-lib-lists.h"

Scheme* lists_thescheme;

SchemeObject* s_find(SchemeObject* proc, SchemeObject* list) {
    while(list != S_EMPTY_LIST) {
	if (lists_thescheme->callProcedure_1(proc, i_car(list)) != S_FALSE)  {
	    return i_car(list);
	}
	list = i_cdr(list);
    }
    return S_FALSE;
}

SchemeObject* s_for_all(int num, SchemeStack::iterator args) {
    wstring procname = L"for-all";
    SchemeObject* proc = args[0];
    assert_arg_type(procname, 1, s_procedure_p, proc);

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
            SchemeObject* arg = s_car(lists_ptr);
            cropped_args[i] = s_cdr(lists_ptr);
            collection.add(arg);
	}

	SchemeObject* result_item = lists_thescheme->callProcedure_n(proc, collection.list);
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
    return lists_thescheme->callProcedure_n(proc, collection.list);
}

SchemeObject* s_exists(int num, SchemeStack::iterator args) {
    wstring procname = L"exists";
    SchemeObject* proc = args[0];
    assert_arg_type(procname, 1, s_procedure_p, proc);

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
            SchemeObject* arg = s_car(lists_ptr);
            cropped_args[i] = s_cdr(lists_ptr);
            collection.add(arg);
	}

	SchemeObject* result_item = lists_thescheme->callProcedure_n(proc, collection.list);
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
    return lists_thescheme->callProcedure_n(proc, collection.list);
}

SchemeObject* s_filter(SchemeObject* proc, SchemeObject* list) {
    vector<SchemeObject*> vec;
    while(list != S_EMPTY_LIST) {
	if (lists_thescheme->callProcedure_1(proc, i_car(list)) != S_FALSE)  {
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

SchemeObject* s_partition(SchemeObject* proc, SchemeObject* list) {
    vector<SchemeObject*> true_vec;
    vector<SchemeObject*> false_vec;
    while(list != S_EMPTY_LIST) {
	if (lists_thescheme->callProcedure_1(proc, i_car(list)) != S_FALSE)  {
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

inline
SchemeObject* member_helper(SchemeObject* (comparator)(SchemeObject*,SchemeObject*), SchemeObject* obj, SchemeObject* p) {
    while (i_null_p(p) == S_FALSE) {
        if ((*comparator)(obj, s_car(p)) == S_TRUE) {
            return p;
        } else {
            p = s_cdr(p);
        }
    }
    return S_FALSE;
}

SchemeObject* s_member(SchemeObject* obj, SchemeObject* p) {
    return member_helper(s_equal_p, obj, p);
}

SchemeObject* s_memq(SchemeObject* obj, SchemeObject* p) {
    return member_helper(s_eq_p, obj, p);
}

SchemeObject* s_memv(SchemeObject* obj, SchemeObject* p) {
    return member_helper(s_eqv_p, obj, p);
}

SchemeObject* s_memp(SchemeObject* proc, SchemeObject* list) {
    while (list != S_EMPTY_LIST) {
	if (lists_thescheme->callProcedure_1(proc, i_car(list)) != S_FALSE)  {
	    return list;
	} else {
	    list = i_cdr(list);
	}
    }
    return S_FALSE;
}

SchemeObject* s_cons_star(int num, SchemeStack::iterator stack) {
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
    lists_thescheme = scheme;
    scheme->assign(L"find"            ,2,0,0, (SchemeObject* (*)()) s_find, envt);
    scheme->assign(L"for-all"         ,2,0,1, (SchemeObject* (*)()) s_for_all, envt);
    scheme->assign(L"exists"          ,2,0,1, (SchemeObject* (*)()) s_exists, envt);
    scheme->assign(L"filter"          ,2,0,0, (SchemeObject* (*)()) s_filter, envt);
    scheme->assign(L"partition"       ,2,0,0, (SchemeObject* (*)()) s_partition, envt);
    scheme->assign(L"memp"            ,2,0,0, (SchemeObject* (*)()) s_memp, envt);
    scheme->assign(L"member"          ,2,0,0, (SchemeObject* (*)()) s_member, envt);
    scheme->assign(L"memq"            ,2,0,0, (SchemeObject* (*)()) s_memq, envt);
    scheme->assign(L"memv"            ,2,0,0, (SchemeObject* (*)()) s_memv, envt);
    scheme->assign(L"cons*"           ,1,0,1, (SchemeObject* (*)()) s_cons_star, envt);
}

