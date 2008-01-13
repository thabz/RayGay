
SchemeObject* s_make_eq_hashtable(SchemeObject* k) {
}

SchemeObject* s_make_eqv_hashtable(SchemeObject* k) {
}

SchemeObject* s_make_hashtable(SchemeObject* hash_func, SchemeObject* equiv, SchemeObject* k) {

}

SchemeObject* s_hashtable_p(SchemeObject* o) {

}

void R6RSLibArithmetic::bind(Scheme* scheme, SchemeObject* envt) {
    scheme->assign(L"make-eq-hashtable"    ,0,1,0, (SchemeObject* (*)()) s_make_eq_hashtable, envt);
    scheme->assign(L"make-eqv-hashtable"   ,0,1,0, (SchemeObject* (*)()) s_make_eqv_hashtable, envt);
    scheme->assign(L"make-hashtable"       ,2,1,0, (SchemeObject* (*)()) s_make_hashtable, envt);
    scheme->assign(L"hashtable?"           ,1,0,0, (SchemeObject* (*)()) s_hashtable_p, envt);
}
