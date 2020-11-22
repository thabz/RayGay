
#include "r6rs-lib-hashtables.h"
#include "numbers.h"

#define DEFAULT_INITIAL_CAPACITY 16

using namespace std;

SchemeObject *equal_p_ptr;
SchemeObject *equal_hash_ptr;

uint32_t i_equal_hash(SchemeObject *o);
void i_hashtable_resize(Scheme *scheme, SchemeObject *hashtable);

SchemeObject *s_make_hashtable(Scheme *scheme, SchemeObject *hash_func,
                               SchemeObject *equiv_func, SchemeObject *k) {
  if (k != S_UNSPECIFIED) {
    assert_arg_positive_int(L"make-hashtable", 1, k);
  }
  int64_t size = k == S_UNSPECIFIED ? DEFAULT_INITIAL_CAPACITY : scm2int(k);
  SchemeObject *buckets = i_make_vector(size, S_EMPTY_LIST);
  return SchemeObject::createHashtable(buckets, hash_func, equiv_func);
}

SchemeObject *s_make_eq_hashtable(Scheme *scheme, SchemeObject *k) {
  if (k != S_UNSPECIFIED) {
    assert_arg_positive_int(L"make-eq-hashtable", 1, k);
  }
  SchemeObject *equiv_func = scheme->lookup(L"eq?");
  SchemeObject *hash_func = scheme->lookup(L"eq-hash");
  return s_make_hashtable(scheme, hash_func, equiv_func, k);
}

SchemeObject *s_make_eqv_hashtable(Scheme *scheme, SchemeObject *k) {
  if (k != S_UNSPECIFIED) {
    assert_arg_positive_int(L"make-eqv-hashtable", 1, k);
  }
  SchemeObject *equiv_func = scheme->lookup(L"eqv?");
  SchemeObject *hash_func = scheme->lookup(L"eqv-hash");
  return s_make_hashtable(scheme, hash_func, equiv_func, k);
}

SchemeObject *s_hashtable_p(Scheme *scheme, SchemeObject *o) {
  return i_hashtable_p(o);
}

// If add is 1 or -1 it's added to the existing size.
// If add is 0 the existing size is set to 0.
void i_hashtable_update_size(Scheme *scheme, SchemeObject *hashtable, int add) {
  SchemeObject *size_pair = i_cddr(hashtable->s_hashtable_meta);
  int64_t size = scm2int(i_car(size_pair));
  if (add == 0) {
    size = 0;
  }
  i_set_car_e(size_pair, int2scm(size + add));

  // Find whether we need to resize the buckets number
  int64_t buckets_num = hashtable->buckets->length;
  if ((add > 0 && size + add > 1.75 * buckets_num) ||
      (add < 0 && size + add < 0.25 * buckets_num)) {
    i_hashtable_resize(scheme, hashtable);
  }
}

SchemeObject *s_hashtable_clear_e(Scheme *scheme, SchemeObject *hashtable,
                                  SchemeObject *k) {
  assert_arg_mutable_hashtable_type(L"hashtable-clear!", 1, hashtable);
  int64_t size;
  if (k == S_UNSPECIFIED) {
    size = DEFAULT_INITIAL_CAPACITY;
  } else {
    size = scm2int(k);
  }
  SchemeObject *buckets = i_make_vector(size, S_EMPTY_LIST);
  hashtable->buckets = buckets;
  i_hashtable_update_size(scheme, hashtable, 0);
  return S_UNSPECIFIED;
}

SchemeObject *s_hashtable_set_e(Scheme *scheme, SchemeObject *hashtable,
                                SchemeObject *key, SchemeObject *obj) {
  assert_arg_mutable_hashtable_type(L"hashtable-set!", 1, hashtable);
  SchemeObject *hash_func = i_car(hashtable->s_hashtable_meta);
  SchemeObject *equiv_func = i_cadr(hashtable->s_hashtable_meta);
  SchemeObject *s_hash = scheme->callProcedure_1(hash_func, key);
  int64_t buckets_num = hashtable->buckets->length;
  int64_t hash = scm2int(s_hash) % buckets_num;
  SchemeObject *bucket_begin = hashtable->buckets->elems[hash];
  SchemeObject *bucket = bucket_begin;
  while (bucket != S_EMPTY_LIST) {
    SchemeObject *entry = i_car(bucket);
    if (scheme->callProcedure_2(equiv_func, key, i_car(entry)) != S_FALSE) {
      i_set_cdr_e(entry, obj);
      return S_UNSPECIFIED;
    }
    bucket = i_cdr(bucket);
  }
  hashtable->buckets->elems[hash] = i_cons(i_cons(key, obj), bucket_begin);
  i_hashtable_update_size(scheme, hashtable, 1);
  return S_UNSPECIFIED;
}

SchemeObject *s_hashtable_ref(Scheme *scheme, SchemeObject *hashtable,
                              SchemeObject *key, SchemeObject *defaul) {
  assert_arg_hashtable_type(L"hashtable-ref", 1, hashtable);
  SchemeObject *hash_func = i_car(hashtable->s_hashtable_meta);
  SchemeObject *equiv_func = i_cadr(hashtable->s_hashtable_meta);
  int64_t hash;
  if (hash_func == equal_hash_ptr) {
    hash = i_equal_hash(key);
  } else {
    hash = scm2int(scheme->callProcedure_1(hash_func, key));
  }
  int64_t buckets_num = hashtable->buckets->length;
  hash = hash % buckets_num;
  SchemeObject *bucket = hashtable->buckets->elems[hash];
  while (bucket != S_EMPTY_LIST) {
    SchemeObject *entry = i_car(bucket);
    if (equiv_func == equal_p_ptr) {
      if (s_equal_p(scheme, key, i_car(entry)) != S_FALSE) {
        return i_cdr(entry);
      }
    } else {
      if (scheme->callProcedure_2(equiv_func, key, i_car(entry)) != S_FALSE) {
        return i_cdr(entry);
      }
    }
    bucket = i_cdr(bucket);
  }
  return defaul;
}

SchemeObject *s_hashtable_update_e(Scheme *scheme, SchemeObject *hashtable,
                                   SchemeObject *key, SchemeObject *proc,
                                   SchemeObject *defaul) {
  assert_arg_mutable_hashtable_type(L"hashtable-update!", 1, hashtable);
  SchemeObject *hash_func = i_car(hashtable->s_hashtable_meta);
  SchemeObject *equiv_func = i_cadr(hashtable->s_hashtable_meta);
  SchemeObject *s_hash = scheme->callProcedure_1(hash_func, key);
  int64_t buckets_num = hashtable->buckets->length;
  int64_t hash = scm2int(s_hash) % buckets_num;
  SchemeObject *bucket_begin = hashtable->buckets->elems[hash];
  SchemeObject *bucket = bucket_begin;
  while (bucket != S_EMPTY_LIST) {
    SchemeObject *entry = i_car(bucket);
    if (scheme->callProcedure_2(equiv_func, key, i_car(entry)) != S_FALSE) {
      SchemeObject *new_val = scheme->callProcedure_1(proc, i_cdr(entry));
      i_set_cdr_e(entry, new_val);
      return S_UNSPECIFIED;
    }
    bucket = i_cdr(bucket);
  }
  SchemeObject *val = scheme->callProcedure_1(proc, defaul);
  hashtable->buckets->elems[hash] = i_cons(i_cons(key, val), bucket_begin);
  i_hashtable_update_size(scheme, hashtable, 1);
  return S_UNSPECIFIED;
}

SchemeObject *s_hashtable_delete_e(Scheme *scheme, SchemeObject *hashtable,
                                   SchemeObject *key) {
  assert_arg_mutable_hashtable_type(L"hashtable-delete!", 1, hashtable);
  SchemeObject *hash_func = i_car(hashtable->s_hashtable_meta);
  SchemeObject *equiv_func = i_cadr(hashtable->s_hashtable_meta);
  SchemeObject *s_hash = scheme->callProcedure_1(hash_func, key);
  int64_t buckets_num = hashtable->buckets->length;
  int64_t hash = scm2int(s_hash) % buckets_num;
  SchemeObject *bucket_begin = hashtable->buckets->elems[hash];
  SchemeObject *bucket = bucket_begin;
  SchemeObject *prev = S_FALSE;
  while (bucket != S_EMPTY_LIST) {
    SchemeObject *entry = i_car(bucket);
    if (scheme->callProcedure_2(equiv_func, key, i_car(entry)) != S_FALSE) {
      if (prev != S_FALSE) {
        i_set_cdr_e(prev, i_cdr(bucket));
      } else {
        hashtable->buckets->elems[hash] = i_cdr(bucket);
      }
      i_hashtable_update_size(scheme, hashtable, -1);
      return S_UNSPECIFIED;
    }
    prev = bucket;
    bucket = i_cdr(bucket);
  }
  return S_UNSPECIFIED;
}

SchemeObject *s_hashtable_contains_p(Scheme *scheme, SchemeObject *hashtable,
                                     SchemeObject *key) {
  assert_arg_hashtable_type(L"hashtable-contains?", 1, hashtable);
  SchemeObject *hash_func = i_car(hashtable->s_hashtable_meta);
  SchemeObject *equiv_func = i_cadr(hashtable->s_hashtable_meta);
  SchemeObject *s_hash = scheme->callProcedure_1(hash_func, key);
  int64_t buckets_num = hashtable->buckets->length;
  int64_t hash = scm2int(s_hash) % buckets_num;
  SchemeObject *bucket = hashtable->buckets->elems[hash];
  while (bucket != S_EMPTY_LIST) {
    SchemeObject *entry = i_car(bucket);
    if (scheme->callProcedure_2(equiv_func, key, i_car(entry)) != S_FALSE) {
      return S_TRUE;
    }
    bucket = i_cdr(bucket);
  }
  return S_FALSE;
}

int64_t i_hashtable_size(SchemeObject *hashtable) {
  return scm2int(i_caddr(hashtable->s_hashtable_meta));
}

SchemeObject *s_hashtable_size(Scheme *scheme, SchemeObject *hashtable) {
  assert_arg_hashtable_type(L"hashtable-size", 1, hashtable);
  return i_caddr(hashtable->s_hashtable_meta);
}

// Double the number of buckets when the hashtable size > 2*buckets.
// Halve the number of buckets when the hashtable size < buckets/2.
// In both cases all entries needs to be re-added with their new hashvalues.
void i_hashtable_resize(Scheme *scheme, SchemeObject *hashtable) {
  int64_t buckets_num = hashtable->buckets->length;
  int64_t entries_num = i_hashtable_size(hashtable);
  int64_t new_buckets_num = buckets_num;
  if (entries_num == 0) {
    new_buckets_num = DEFAULT_INITIAL_CAPACITY;
  } else if (entries_num < buckets_num / 2) {
    new_buckets_num = buckets_num / 2;
  } else if (entries_num > buckets_num * 2) {
    new_buckets_num = buckets_num * 2;
  }
  if (new_buckets_num == buckets_num) {
    return;
  }

  // cout << "Resizing hash to size " << new_buckets_num << endl;
  SchemeObject *hash_func = i_car(hashtable->s_hashtable_meta);
  int64_t hashes[entries_num];
  int64_t index = 0;

  // First find all the hashes. We do this before building the new bucket-lists
  // as the calls into Scheme might incur a garbage-collection.
  for (uint32_t i = 0; i < buckets_num; i++) {
    SchemeObject *bucket = hashtable->buckets->elems[i];
    while (bucket != S_EMPTY_LIST) {
      SchemeObject *entry = i_car(bucket);
      SchemeObject *s_hash = scheme->callProcedure_1(hash_func, i_car(entry));
      hashes[index++] = scm2int(s_hash) % new_buckets_num;
      bucket = i_cdr(bucket);
    }
  }

  // Build the new buckets-lists
  index = 0;
  SchemeObject *new_buckets = i_make_vector(new_buckets_num, S_EMPTY_LIST);
  for (uint32_t i = 0; i < buckets_num; i++) {
    SchemeObject *bucket = hashtable->buckets->elems[i];
    while (bucket != S_EMPTY_LIST) {
      SchemeObject *entry = i_car(bucket);
      int64_t hash = hashes[index];
      new_buckets->elems[hash] = i_cons(entry, new_buckets->elems[hash]);
      index++;
      bucket = i_cdr(bucket);
    }
  }
  hashtable->buckets = new_buckets;
}

SchemeObject *s_hashtable_keys(Scheme *scheme, SchemeObject *hashtable) {
  assert_arg_hashtable_type(L"hashtable-clear!", 1, hashtable);
  int64_t entries_num = i_hashtable_size(hashtable);
  SchemeObject *keys = i_make_vector(entries_num, S_UNSPECIFIED);
  SchemeObject *buckets = hashtable->buckets;
  uint64_t j = 0;
  for (uint32_t i = 0; i < buckets->length; i++) {
    SchemeObject *bucket = buckets->elems[i];
    while (bucket != S_EMPTY_LIST) {
      SchemeObject *entry = i_car(bucket);
      keys->elems[j++] = i_car(entry);
      bucket = i_cdr(bucket);
    }
  }
  return keys;
}

SchemeObject *s_hashtable_entries(Scheme *scheme, SchemeObject *hashtable) {
  assert_arg_hashtable_type(L"hashtable-entries", 1, hashtable);
  int64_t entries_num = i_hashtable_size(hashtable);
  SchemeObject *keys = i_make_vector(entries_num, S_UNSPECIFIED);
  SchemeObject *values = i_make_vector(entries_num, S_UNSPECIFIED);
  SchemeObject *buckets = hashtable->buckets;
  uint64_t j = 0;
  for (uint32_t i = 0; i < buckets->length; i++) {
    SchemeObject *bucket = buckets->elems[i];
    while (bucket != S_EMPTY_LIST) {
      SchemeObject *entry = i_car(bucket);
      keys->elems[j] = i_car(entry);
      values->elems[j] = i_cdr(entry);
      j++;
      bucket = i_cdr(bucket);
    }
  }
  return i_list_2(keys, values);
}

SchemeObject *s_hashtable_copy(Scheme *scheme, SchemeObject *hashtable,
                               SchemeObject *mutab) {
  assert_arg_hashtable_type(L"hashtable-copy", 1, hashtable);

  SchemeObject *hash_func = i_car(hashtable->s_hashtable_meta);
  SchemeObject *equiv_func = i_cadr(hashtable->s_hashtable_meta);
  SchemeObject *buckets = hashtable->buckets;

  SchemeObject *copy =
      s_make_hashtable(scheme, hash_func, equiv_func, int2scm(buckets->length));

  // Copy the buckets
  // Note: The copy-buckets are reversed.
  for (uint32_t i = 0; i < buckets->length; i++) {
    SchemeObject *bucket = buckets->elems[i];
    SchemeObject *bucket_copy = S_EMPTY_LIST;
    while (bucket != S_EMPTY_LIST) {
      SchemeObject *e = i_car(bucket);
      bucket_copy = i_cons(i_cons(i_car(e), i_cdr(e)), bucket_copy);
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

SchemeObject *s_hashtable_equivalence_function(Scheme *scheme,
                                               SchemeObject *hashtable) {
  assert_arg_hashtable_type(L"hashtable-equivalence-function", 1, hashtable);
  return i_cadr(hashtable->s_hashtable_meta);
}

SchemeObject *s_hashtable_hash_function(Scheme *scheme,
                                        SchemeObject *hashtable) {
  assert_arg_hashtable_type(L"hashtable-hash-function", 1, hashtable);
  return i_car(hashtable->s_hashtable_meta);
}

SchemeObject *s_hashtable_mutable_p(Scheme *scheme, SchemeObject *hashtable) {
  assert_arg_hashtable_type(L"hashtable-mutable?", 1, hashtable);
  return hashtable->immutable() ? S_FALSE : S_TRUE;
}

////////////////////////////////////////////////////////////////////////////////////
// Hashing functions
////////////////////////////////////////////////////////////////////////////////////

uint32_t rehash(uint32_t h) {
  h += ~(h << 9);
  h ^= (h >> 14);
  h += (h << 4);
  h ^= (h >> 10);
  return h;
}

uint32_t i_string_hash(std::wstring str) {
  uint32_t h = 1;
  for (uint32_t i = 0; i < str.size(); i++) {
    h *= str[i] + 37;
    h ^= h << 3;
    h += h >> 5;
    h ^= h << 4;
    h += h >> 17;
    h ^= h << 25;
    h += h >> 6;
  }
  return h;
}

uint32_t i_string_ci_hash(std::wstring str) {
  uint32_t h = 1;
  for (uint32_t i = 0; i < str.size(); i++) {
    h *= ::towlower(str[i]) + 37;
  }
  return h;
}

inline uint32_t i_pointer_hash(void *ptr) {
  int32_t h = intptr_t(ptr) & 0xffffffff;
  h ^= h << 3;
  h += h >> 5;
  h ^= h << 4;
  h += h >> 17;
  h ^= h << 25;
  h += h >> 6;
  return (h < 0) ? -h : h;
}

inline uint32_t i_symbol_hash(SchemeObject *s) { return i_pointer_hash(s); }

inline uint32_t i_int_hash(uint64_t v) {
  uint64_t h = v ^ (v >> 32);
  h += ~(h << 9);
  h ^= (h >> 14);
  h += (h << 4);
  h ^= (h >> 10);
  return h;
}

inline uint32_t i_double_hash(double d) {
  union {
    double dd;
    uint64_t ii;
  } u;
  u.dd = d;
  return u.ii ^ (u.ii >> 32);
}

inline uint32_t i_rational_hash(rational_type r) {
  return i_int_hash(r.numerator()) ^ i_int_hash(r.denominator());
}

inline uint32_t i_complex_hash(std::complex<double> c) {
  return i_double_hash(c.real()) ^ i_double_hash(c.imag());
  ;
}

SchemeObject *s_string_hash(Scheme *scheme, SchemeObject *o) {
  assert_arg_string_type(L"string-hash", 1, o);
  return uint2scm(i_string_hash(o->str));
}

SchemeObject *s_string_ci_hash(Scheme *scheme, SchemeObject *o) {
  assert_arg_string_type(L"string-ci-hash", 1, o);
  return uint2scm(i_string_ci_hash(o->str));
}

SchemeObject *s_symbol_hash(Scheme *scheme, SchemeObject *o) {
  assert_arg_symbol_type(L"symbol-hash", 1, o);
  return uint2scm(i_symbol_hash(o));
}

uint32_t i_equal_hash(SchemeObject *o) {
  SchemeObject::ObjectType t = o->type();
  uint32_t result;
  if (t == SchemeObject::INTEGER_NUMBER) {
    result = i_int_hash(scm2int(o));
  } else if (t == SchemeObject::PAIR) {
    result = 13371;
    while (i_pair_p(o) == S_TRUE) {
      result += 37 * i_equal_hash(i_car(o));
      o = i_cdr(o);
    }
    result += 37 * i_equal_hash(o);
  } else if (o == S_EMPTY_LIST) {
    result = 9873;
  } else if (t == SchemeObject::BOOL) {
    result = o == S_TRUE ? 1231 : 1237; // As per Java
  } else if (t == SchemeObject::RATIONAL_NUMBER) {
    result = i_rational_hash(scm2rational(o));
  } else if (t == SchemeObject::REAL_NUMBER) {
    result = i_double_hash(scm2double(o));
  } else if (t == SchemeObject::COMPLEX_NUMBER) {
    result = i_complex_hash(scm2complex(o));
  } else if (t == SchemeObject::STRING) {
    result = i_string_hash(o->str);
  } else if (t == SchemeObject::SYMBOL) {
    result = i_symbol_hash(o);
  } else {
    result = i_string_hash(o->toString());
  }
  return result;
}

SchemeObject *s_equal_hash(Scheme *scheme, SchemeObject *o) {
  return uint2scm(i_equal_hash(o));
}

SchemeObject *s_eq_hash(Scheme *scheme, SchemeObject *o) {
  return uint2scm(i_pointer_hash(o));
}

SchemeObject *s_eqv_hash(Scheme *scheme, SchemeObject *o) {
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

void R6RSLibHashtables::bind(Scheme *scheme, SchemeObject *envt) {
  scheme->assign(L"eq-hash", 1, 0, 0, (SchemeObject * (*)()) s_eq_hash, envt);
  scheme->assign(L"eqv-hash", 1, 0, 0, (SchemeObject * (*)()) s_eqv_hash, envt);
  scheme->assign(L"equal-hash", 1, 0, 0, (SchemeObject * (*)()) s_equal_hash,
                 envt);
  scheme->assign(L"string-hash", 1, 0, 0, (SchemeObject * (*)()) s_string_hash,
                 envt);
  scheme->assign(L"string-ci-hash", 1, 0, 0,
                 (SchemeObject * (*)()) s_string_ci_hash, envt);
  scheme->assign(L"symbol-hash", 1, 0, 0, (SchemeObject * (*)()) s_symbol_hash,
                 envt);
  scheme->assign(L"make-eq-hashtable", 0, 1, 0,
                 (SchemeObject * (*)()) s_make_eq_hashtable, envt);
  scheme->assign(L"make-eqv-hashtable", 0, 1, 0,
                 (SchemeObject * (*)()) s_make_eqv_hashtable, envt);
  scheme->assign(L"make-hashtable", 2, 1, 0,
                 (SchemeObject * (*)()) s_make_hashtable, envt);
  scheme->assign(L"hashtable?", 1, 0, 0, (SchemeObject * (*)()) s_hashtable_p,
                 envt);
  scheme->assign(L"hashtable-size", 1, 0, 0,
                 (SchemeObject * (*)()) s_hashtable_size, envt);
  scheme->assign(L"hashtable-ref", 3, 0, 0,
                 (SchemeObject * (*)()) s_hashtable_ref, envt);
  scheme->assign(L"hashtable-set!", 3, 0, 0,
                 (SchemeObject * (*)()) s_hashtable_set_e, envt);
  scheme->assign(L"hashtable-delete!", 2, 0, 0,
                 (SchemeObject * (*)()) s_hashtable_delete_e, envt);
  scheme->assign(L"hashtable-update!", 4, 0, 0,
                 (SchemeObject * (*)()) s_hashtable_update_e, envt);
  scheme->assign(L"hashtable-contains?", 2, 0, 0,
                 (SchemeObject * (*)()) s_hashtable_contains_p, envt);
  scheme->assign(L"hashtable-clear!", 1, 1, 0,
                 (SchemeObject * (*)()) s_hashtable_clear_e, envt);
  scheme->assign(L"hashtable-copy", 1, 1, 0,
                 (SchemeObject * (*)()) s_hashtable_copy, envt);
  scheme->assign(L"hashtable-keys", 1, 0, 0,
                 (SchemeObject * (*)()) s_hashtable_keys, envt);
  scheme->assign(L"hashtable-entries", 1, 0, 0,
                 (SchemeObject * (*)()) s_hashtable_entries, envt);
  scheme->assign(L"hashtable-equivalence-function", 1, 0, 0,
                 (SchemeObject * (*)()) s_hashtable_equivalence_function, envt);
  scheme->assign(L"hashtable-hash-function", 1, 0, 0,
                 (SchemeObject * (*)()) s_hashtable_hash_function, envt);
  scheme->assign(L"hashtable-mutable?", 1, 0, 0,
                 (SchemeObject * (*)()) s_hashtable_mutable_p, envt);

  equal_hash_ptr = scheme->lookup(L"equal-hash");
  equal_p_ptr = scheme->lookup(L"equal?");
}
