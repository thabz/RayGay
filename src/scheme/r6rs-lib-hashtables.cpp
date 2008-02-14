
#include "r6rs-lib-hashtables.h"
#include "numbers.h";

using namespace std;

Scheme* myscheme;

SchemeObject* s_make_hashtable(SchemeObject* hash_func, SchemeObject* equiv_func, SchemeObject* k) {
    if (k != S_UNSPECIFIED) {
        assert_arg_positive_int(L"make-hashtable",1,k);
    }
    int64_t size;
    if (k == S_UNSPECIFIED) {
        size = 256;
    } else {
        size = scm2int(k);
    }
    SchemeObject* buckets = i_make_vector(size, S_EMPTY_LIST);
    return SchemeObject::createHashtable(buckets, hash_func, equiv_func);
}

SchemeObject* s_make_eq_hashtable(SchemeObject* k) {
    if (k != S_UNSPECIFIED) {
        assert_arg_positive_int(L"make-eq-hashtable",1,k);
    }
    SchemeObject* equiv_func = myscheme->lookup(L"eq?");
    SchemeObject* hash_func = myscheme->lookup(L"eq-hash");
    return s_make_hashtable(hash_func, equiv_func, k);
}

SchemeObject* s_make_eqv_hashtable(SchemeObject* k) {
    if (k != S_UNSPECIFIED) {
        assert_arg_positive_int(L"make-eqv-hashtable",1,k);
    }
    SchemeObject* equiv_func = myscheme->lookup(L"eqv?");
    SchemeObject* hash_func = myscheme->lookup(L"eqv-hash");
    return s_make_hashtable(hash_func, equiv_func, k);
}

SchemeObject* s_hashtable_p(SchemeObject* o) {
    return i_hashtable_p(o);
}

SchemeObject* s_hashtable_clear_e(SchemeObject* hashtable, SchemeObject* k) {
    assert_arg_mutable_hashtable_type(L"hashtable-clear!", 1, hashtable);
    int64_t size;
    if (k == S_UNSPECIFIED) {
        size = 256;
    } else {
        size = scm2int(k);
    }
    SchemeObject* buckets = i_make_vector(size, S_EMPTY_LIST);
    hashtable->buckets = buckets;
    return S_UNSPECIFIED;
}

SchemeObject* s_hashtable_set_e(SchemeObject* hashtable, SchemeObject* key, SchemeObject* obj) {
    assert_arg_mutable_hashtable_type(L"hashtable-set!", 1, hashtable);
    SchemeObject* hash_func  = i_car(hashtable->s_hashtable_funcs);
    SchemeObject* equiv_func = i_cdr(hashtable->s_hashtable_funcs);
    SchemeObject* s_hash = myscheme->callProcedure_1(hash_func, key);
    int64_t buckets_num = hashtable->buckets->length;
    int64_t hash = scm2int(s_hash) % buckets_num;
    SchemeObject* bucket_begin = hashtable->buckets->elems[hash];
    SchemeObject* bucket = bucket_begin;
    while (bucket != S_EMPTY_LIST) {
        SchemeObject* entry = i_car(bucket);
        if (myscheme->callProcedure_2(equiv_func, key, i_car(entry)) != S_FALSE) {
            i_set_cdr_e(entry, obj);
            return S_UNSPECIFIED;
        }
        bucket = i_cdr(bucket);
    }
    hashtable->buckets->elems[hash] = i_cons(i_cons(key,obj), bucket_begin);
    return S_UNSPECIFIED;
}

SchemeObject* s_hashtable_ref(SchemeObject* hashtable, SchemeObject* key, SchemeObject* defaul) {
    assert_arg_hashtable_type(L"hashtable-ref", 1, hashtable);
    SchemeObject* hash_func  = i_car(hashtable->s_hashtable_funcs);
    SchemeObject* equiv_func = i_cdr(hashtable->s_hashtable_funcs);
    SchemeObject* s_hash = myscheme->callProcedure_1(hash_func, key);
    int64_t buckets_num = hashtable->buckets->length;
    int64_t hash = scm2int(s_hash) % buckets_num;
    SchemeObject* bucket = hashtable->buckets->elems[hash];
    while (bucket != S_EMPTY_LIST) {
        SchemeObject* entry = i_car(bucket);
        if (myscheme->callProcedure_2(equiv_func, key, i_car(entry)) != S_FALSE) {
            return i_cdr(entry);
        }
        bucket = i_cdr(bucket);
    }
    return defaul;
}

SchemeObject* s_hashtable_update_e(SchemeObject* hashtable, SchemeObject* key, SchemeObject* proc, SchemeObject* defaul) {
    assert_arg_mutable_hashtable_type(L"hashtable-update!", 1, hashtable);
    SchemeObject* hash_func  = i_car(hashtable->s_hashtable_funcs);
    SchemeObject* equiv_func = i_cdr(hashtable->s_hashtable_funcs);
    SchemeObject* s_hash = myscheme->callProcedure_1(hash_func, key);
    int64_t buckets_num = hashtable->buckets->length;
    int64_t hash = scm2int(s_hash) % buckets_num;
    SchemeObject* bucket_begin = hashtable->buckets->elems[hash];
    SchemeObject* bucket = bucket_begin;
    while (bucket != S_EMPTY_LIST) {
        SchemeObject* entry = i_car(bucket);
        if (myscheme->callProcedure_2(equiv_func, key, i_car(entry)) != S_FALSE) {
	    SchemeObject* new_val = myscheme->callProcedure_1(proc, i_cdr(entry));
            i_set_cdr_e(entry, new_val);
            return S_UNSPECIFIED;
        }
        bucket = i_cdr(bucket);
    }
    SchemeObject* val = myscheme->callProcedure_1(proc, defaul);
    hashtable->buckets->elems[hash] = i_cons(i_cons(key,val), bucket_begin);
    return S_UNSPECIFIED;
}

SchemeObject* s_hashtable_delete_e(SchemeObject* hashtable, SchemeObject* key) {
    assert_arg_mutable_hashtable_type(L"hashtable-delete!", 1, hashtable);
    SchemeObject* hash_func  = i_car(hashtable->s_hashtable_funcs);
    SchemeObject* equiv_func = i_cdr(hashtable->s_hashtable_funcs);
    SchemeObject* s_hash = myscheme->callProcedure_1(hash_func, key);
    int64_t buckets_num = hashtable->buckets->length;
    int64_t hash = scm2int(s_hash) % buckets_num;
    SchemeObject* bucket_begin = hashtable->buckets->elems[hash];
    SchemeObject* bucket = bucket_begin;
    SchemeObject* prev = S_FALSE;
    while (bucket != S_EMPTY_LIST) {
        SchemeObject* entry = i_car(bucket);
        if (myscheme->callProcedure_2(equiv_func, key, i_car(entry)) != S_FALSE) {
	    if (prev != S_FALSE) {
		i_set_cdr_e(prev, i_cdr(bucket));
	    } else {
		hashtable->buckets->elems[hash] = i_cdr(bucket);
	    }
	    return S_UNSPECIFIED;
        }
	prev = bucket;
        bucket = i_cdr(bucket);
    }
    return S_UNSPECIFIED;
}

SchemeObject* s_hashtable_contains_p(SchemeObject* hashtable, SchemeObject* key) {
    assert_arg_hashtable_type(L"hashtable-contains?", 1, hashtable);
    SchemeObject* hash_func  = i_car(hashtable->s_hashtable_funcs);
    SchemeObject* equiv_func = i_cdr(hashtable->s_hashtable_funcs);
    SchemeObject* s_hash = myscheme->callProcedure_1(hash_func, key);
    int64_t buckets_num = hashtable->buckets->length;
    int64_t hash = scm2int(s_hash) % buckets_num;
    SchemeObject* bucket = hashtable->buckets->elems[hash];
    while (bucket != S_EMPTY_LIST) {
        SchemeObject* entry = i_car(bucket);
        if (myscheme->callProcedure_2(equiv_func, key, i_car(entry)) != S_FALSE) {
            return S_TRUE;
        }
        bucket = i_cdr(bucket);
    }
    return S_FALSE;
}

int64_t i_hashtable_size(SchemeObject* hashtable) {
    SchemeObject* buckets = hashtable->buckets;
    int64_t result = 0;
    for(int32_t i = 0; i < buckets->length; i++) {
        result += i_length(buckets->elems[i]);
    }
    return result;
}

SchemeObject* s_hashtable_size(SchemeObject* hashtable) {
    assert_arg_hashtable_type(L"hashtable-size", 1, hashtable);
    return int2scm(i_hashtable_size(hashtable));
}

SchemeObject* s_hashtable_keys(SchemeObject* hashtable) {
    assert_arg_hashtable_type(L"hashtable-clear!", 1, hashtable);
    int64_t entries_num = i_hashtable_size(hashtable);
    SchemeObject* keys = i_make_vector(entries_num, S_UNSPECIFIED);
    SchemeObject* buckets = hashtable->buckets;
    uint64_t j = 0;
    for(int32_t i = 0; i < buckets->length; i++) {
        SchemeObject* bucket = buckets->elems[i];
        while (bucket != S_EMPTY_LIST) {
            SchemeObject* entry = i_car(bucket);
            keys->elems[j++] = i_car(entry);
            bucket = i_cdr(bucket);
        }
    }
    return keys;
}

SchemeObject* s_hashtable_entries(SchemeObject* hashtable) {
    assert_arg_hashtable_type(L"hashtable-entries", 1, hashtable);
    int64_t entries_num = i_hashtable_size(hashtable);
    SchemeObject* keys = i_make_vector(entries_num, S_UNSPECIFIED);
    SchemeObject* values = i_make_vector(entries_num, S_UNSPECIFIED);
    SchemeObject* buckets = hashtable->buckets;
    uint64_t j = 0;
    for(int32_t i = 0; i < buckets->length; i++) {
        SchemeObject* bucket = buckets->elems[i];
        while (bucket != S_EMPTY_LIST) {
            SchemeObject* entry = i_car(bucket);
            keys->elems[j] = i_car(entry);
            values->elems[j] = i_cdr(entry);
            j++;
            bucket = i_cdr(bucket);
        }
    }
    return i_list_2(keys,values);
}

SchemeObject* s_hashtable_copy(SchemeObject* hashtable, SchemeObject* mutab) {
    assert_arg_hashtable_type(L"hashtable-copy", 1, hashtable);

    SchemeObject* hash_func  = i_car(hashtable->s_hashtable_funcs);
    SchemeObject* equiv_func = i_cdr(hashtable->s_hashtable_funcs);
    SchemeObject* buckets = hashtable->buckets;

    SchemeObject* copy = s_make_hashtable(hash_func, equiv_func, int2scm(buckets->length));

    // Copy the buckets
    // Note: The copy-buckets are reversed.
    for(int32_t i = 0; i < buckets->length; i++) {
        SchemeObject* bucket = buckets->elems[i];
	SchemeObject* bucket_copy = S_EMPTY_LIST;
        while (bucket != S_EMPTY_LIST) {
            SchemeObject* e = i_car(bucket);
	    bucket_copy = i_cons(i_cons(i_car(e),i_cdr(e)), bucket_copy);
            bucket = i_cdr(bucket);
        }
	copy->buckets->elems[i] = bucket_copy;
    }

    // Set the immutable flag
    if (mutab == S_UNSPECIFIED || mutab == S_FALSE) {
	copy->set_immutable(true);
    }
    return copy;
}


SchemeObject* s_hashtable_equivalence_function(SchemeObject* hashtable) {
    assert_arg_hashtable_type(L"hashtable-equivalence-function", 1, hashtable);
    return i_cdr(hashtable->s_hashtable_funcs);
}

SchemeObject* s_hashtable_hash_function(SchemeObject* hashtable) {
    assert_arg_hashtable_type(L"hashtable-hash-function", 1, hashtable);
    return i_car(hashtable->s_hashtable_funcs);
}

SchemeObject* s_hashtable_mutable_p(SchemeObject* hashtable) {
    assert_arg_hashtable_type(L"hashtable-mutable?", 1, hashtable);
    return hashtable->immutable() ? S_FALSE : S_TRUE;
}

////////////////////////////////////////////////////////////////////////////////////
// Hashing functions
////////////////////////////////////////////////////////////////////////////////////

uint32_t i_string_hash(std::wstring str) {
    uint32_t h = 1;
    for(uint32_t i = 0; i < str.size(); i++) {
        h *= str[i] + 37;
        h ^= h << 3;
        h += h >> 5;
        h ^= h << 4;
        h += h >> 17;
        h ^= h << 25;
        h += h >> 6;
    }
    return (h < 0) ? -h : h;
}

uint32_t i_string_ci_hash(std::wstring str) {
    uint32_t h = 1;
    for(uint32_t i = 0; i < str.size(); i++) {
        h *= ::towlower(str[i]) + 37;
    }
    return h;
}

uint32_t i_pointer_hash(void* ptr) {
    int32_t h = int32_t(ptr);
    h ^= h << 3;
    h += h >> 5;
    h ^= h << 4;
    h += h >> 17;
    h ^= h << 25;
    h += h >> 6;
    return (h < 0) ? -h : h;
}

uint32_t i_int_hash(uint64_t v) {
    return v^(v >> 32);
}

uint32_t i_double_hash(double d) {
    union {
        double dd;
        uint64_t ii;
    } u;
    u.dd = d;
    return u.ii^(u.ii >> 32);
}

uint32_t i_rational_hash(rational_type r) {
    return i_int_hash(r.numerator()) ^ i_int_hash(r.denominator());
}

uint32_t i_complex_hash(std::complex<double> c) {
    return i_double_hash(c.real()) ^ i_double_hash(c.imag());;
}


SchemeObject* s_string_hash(SchemeObject* o) {
    assert_arg_string_type(L"string-hash", 1, o);
    // TODO: Mangle the results
    return uint2scm(i_string_hash(o->str));
}

SchemeObject* s_string_ci_hash(SchemeObject* o) {
    assert_arg_string_type(L"string-ci-hash", 1, o);
    // TODO: Mangle the results
    return uint2scm(i_string_ci_hash(o->str));
}

SchemeObject* s_symbol_hash(SchemeObject* o) {
    assert_arg_symbol_type(L"symbol-hash", 1, o);
    return uint2scm(i_pointer_hash(o));
}

SchemeObject* s_equal_hash(SchemeObject* o) {
    // TODO: Mangle the results
    SchemeObject::ObjectType t = o->type();
    uint32_t result;
    if (t == SchemeObject::INTEGER_NUMBER) {
        result = i_int_hash(scm2int(o));
    } else if (t == SchemeObject::PAIR) {
        // TODO: Recursion will kill stack on big lists
        return int2scm(scm2int(s_equal_hash(i_car(o))) + 37 * scm2int(s_equal_hash(i_cdr(o))));    
    } else if (o == S_EMPTY_LIST) {
        return int2scm(9873);
    } else if (t == SchemeObject::BOOL) {
        result = o == S_TRUE ? 1231 : 1237; // As per Java
    } else if (t == SchemeObject::RATIONAL_NUMBER) {
        result = i_rational_hash(scm2rational(o));
    } else if (t == SchemeObject::REAL_NUMBER) {
        result = i_double_hash(scm2double(o));
    } else if (t == SchemeObject::COMPLEX_NUMBER) {
        result = i_complex_hash(scm2complex(o));
    } else if (t == SchemeObject::STRING) {
        return s_string_hash(o);    
    } else if (t == SchemeObject::SYMBOL) {
        return s_symbol_hash(o);    
    } else {
        result = i_string_hash(o->toString());
    }
    return uint2scm(result);
}

SchemeObject* s_eq_hash(SchemeObject* o) {
    return uint2scm(i_pointer_hash(o));
}

SchemeObject* s_eqv_hash(SchemeObject* o) {
    SchemeObject::ObjectType t = o->type();
    uint32_t result;
    if (t == SchemeObject::INTEGER_NUMBER) {
        result = i_int_hash(scm2int(o));
    } else if (t == SchemeObject::CHAR) {
    	result = o->c;
    } else if (t == SchemeObject::BOOL) {
        result = o == S_TRUE ? 1231 : 1237; // As per Java
    } else if (t == SchemeObject::RATIONAL_NUMBER) {
        result = i_rational_hash(scm2rational(o));
    } else if (t == SchemeObject::REAL_NUMBER) {
        result = i_double_hash(scm2double(o));
    } else if (t == SchemeObject::COMPLEX_NUMBER) {
        result = i_complex_hash(scm2complex(o));
    } else {
        result = i_pointer_hash(o);
    }
    return uint2scm(result);
}

void R6RSLibHashtables::bind(Scheme* scheme, SchemeObject* envt) {
    myscheme = scheme;
    scheme->assign(L"eq-hash"              ,1,0,0, (SchemeObject* (*)()) s_eq_hash, envt);
    scheme->assign(L"eqv-hash"             ,1,0,0, (SchemeObject* (*)()) s_eqv_hash, envt);
    scheme->assign(L"equal-hash"           ,1,0,0, (SchemeObject* (*)()) s_equal_hash, envt);
    scheme->assign(L"string-hash"          ,1,0,0, (SchemeObject* (*)()) s_string_hash, envt);
    scheme->assign(L"string-ci-hash"       ,1,0,0, (SchemeObject* (*)()) s_string_ci_hash, envt);
    scheme->assign(L"symbol-hash"          ,1,0,0, (SchemeObject* (*)()) s_symbol_hash, envt);
    scheme->assign(L"make-eq-hashtable"    ,0,1,0, (SchemeObject* (*)()) s_make_eq_hashtable, envt);
    scheme->assign(L"make-eqv-hashtable"   ,0,1,0, (SchemeObject* (*)()) s_make_eqv_hashtable, envt);
    scheme->assign(L"make-hashtable"       ,2,1,0, (SchemeObject* (*)()) s_make_hashtable, envt);
    scheme->assign(L"hashtable?"           ,1,0,0, (SchemeObject* (*)()) s_hashtable_p, envt);
    scheme->assign(L"hashtable-size"       ,1,0,0, (SchemeObject* (*)()) s_hashtable_size, envt);
    scheme->assign(L"hashtable-ref"        ,3,0,0, (SchemeObject* (*)()) s_hashtable_ref, envt);
    scheme->assign(L"hashtable-set!"       ,3,0,0, (SchemeObject* (*)()) s_hashtable_set_e, envt);
    scheme->assign(L"hashtable-delete!"    ,2,0,0, (SchemeObject* (*)()) s_hashtable_delete_e, envt);
    scheme->assign(L"hashtable-update!"    ,4,0,0, (SchemeObject* (*)()) s_hashtable_update_e, envt);
    scheme->assign(L"hashtable-contains?"  ,2,0,0, (SchemeObject* (*)()) s_hashtable_contains_p, envt);
    scheme->assign(L"hashtable-clear!"     ,1,1,0, (SchemeObject* (*)()) s_hashtable_clear_e, envt);
    scheme->assign(L"hashtable-copy"       ,1,1,0, (SchemeObject* (*)()) s_hashtable_copy, envt);
    scheme->assign(L"hashtable-keys"       ,1,0,0, (SchemeObject* (*)()) s_hashtable_keys, envt);
    scheme->assign(L"hashtable-entries"    ,1,0,0, (SchemeObject* (*)()) s_hashtable_entries, envt);
    scheme->assign(L"hashtable-equivalence-function",1,0,0, (SchemeObject* (*)()) s_hashtable_equivalence_function, envt);
    scheme->assign(L"hashtable-hash-function",1,0,0, (SchemeObject* (*)()) s_hashtable_hash_function, envt);
    scheme->assign(L"hashtable-mutable?"   ,1,0,0, (SchemeObject* (*)()) s_hashtable_mutable_p, envt);
}
