#include "r6rs-lib-bytevectors.h"
#include "scheme.h"
#include "numbers.h"
#include <config.h>

SchemeObject* little_symbol;
SchemeObject* big_symbol;

SchemeObject* s_native_endianness(Scheme* scheme) {
#if WORDS_BIGENDIAN
    return big_symbol;
#else
    return little_symbol;
#endif
}

SchemeObject* s_bytevector_p(Scheme* scheme, SchemeObject* o) {
    return bool2scm(i_bytevector_p(o));
}

SchemeObject* s_make_bytevector(Scheme* scheme, SchemeObject* s_k, SchemeObject* s_fill) {
    assert_arg_non_negative_int(L"make-bytevector",1, s_k);
    uint32_t k = scm2int(s_k);
    uint8_t* bytes = new uint8_t[k];
    
    if (s_fill != S_UNSPECIFIED) {
        assert_arg_int_in_range(L"make-bytevector", 2, s_fill, -128, 255);
        int64_t f = scm2int(s_fill);
        uint8_t fill = f < 0 ? 256 + f : f;
        for(uint32_t i = 0; i < k; i++) {
            bytes[i] = fill;
        }
    }
    
    return SchemeObject::createBytevector(bytes, k);
}

SchemeObject* s_bytevector_length(Scheme* scheme, SchemeObject* bytevector) {
    assert_arg_bytevector_type(L"bytevector-length", 1, bytevector);
    return int2scm(bytevector->length);
}

SchemeObject* s_bytevector_equal_p(Scheme* scheme, SchemeObject* b1, SchemeObject* b2) {
    assert_arg_bytevector_type(L"bytevector=?", 1, b1);
    assert_arg_bytevector_type(L"bytevector=?", 2, b2);
    if (b1->length != b2->length) return S_FALSE;
    for(uint32_t i = 0; i < b1->length; i++) {
        if (b1->bytevector[i] != b2->bytevector[i]) return S_FALSE;
    }
    return S_TRUE;
}

SchemeObject* s_bytevector_fill_e(Scheme* scheme, SchemeObject* bytevector, SchemeObject* s_fill) {
    assert_arg_bytevector_type(L"bytevector-fill!", 1, bytevector);
    assert_arg_not_immutable(L"bytevector-fill!", 1, bytevector);
    assert_arg_int_in_range(L"make-bytevector", 2, s_fill, -128, 255);
    int64_t f = scm2int(s_fill);
    uint8_t fill = f < 0 ? 256 + f : f;
    for(uint32_t i = 0; i < bytevector->length; i++) {
        bytevector->bytevector[i] = fill;
    }
    return S_UNSPECIFIED;
}

SchemeObject* s_bytevector_copy_e(Scheme* scheme, SchemeObject* source, SchemeObject* s_source_start, 
                                  SchemeObject* target, SchemeObject* s_target_start, 
                                  SchemeObject* s_k) {
    assert_arg_bytevector_type(L"bytevector-copy!", 1, source);
    assert_arg_bytevector_type(L"bytevector-copy!", 3, target);
    assert_arg_not_immutable(L"bytevector-copy!", 3, target);
    assert_arg_non_negative_int(L"bytevector-copy!",5, s_k);
    int64_t k = scm2int(s_k);
    assert_arg_int_in_range(L"bytevector-copy!", 2, s_source_start, 0, source->length-1-k);
    assert_arg_int_in_range(L"bytevector-copy!", 4, s_target_start, 0, target->length-1-k);
    
    uint32_t source_start = scm2int(s_source_start);
    uint32_t target_start = scm2int(s_target_start);

    if (source_start < target_start) {
        // Reverse copy when possible overlapping
        for(int32_t i = k-1; i >= 0; i--) {
            target->bytevector[target_start+i] = source->bytevector[source_start+i];
        }
    } else {
        for(int32_t i = 0; i < k; i++) {
            target->bytevector[target_start+i] = source->bytevector[source_start+i];
        }
    }
    return S_UNSPECIFIED;
}


SchemeObject* s_bytevector_copy(Scheme* scheme, SchemeObject* bytevector) {
    assert_arg_bytevector_type(L"bytevector-copy", 1, bytevector);

    uint8_t* copy = new uint8_t[bytevector->length];
    for(uint32_t i = 0; i < bytevector->length; i++) {
        copy[i] = bytevector->bytevector[i];
    }
    return SchemeObject::createBytevector(copy, bytevector->length);
}

SchemeObject* s_bytevector_u8_ref(Scheme* scheme, SchemeObject* bytevector, SchemeObject* k) {
    assert_arg_bytevector_type(L"bytevector-u8-ref", 1, bytevector);
    assert_arg_int_in_range(L"bytevector-u8-ref", 2, k, 0, bytevector->length-1);
    return uint2scm(bytevector->bytevector[scm2int(k)]);
}

SchemeObject* s_bytevector_s8_ref(Scheme* scheme, SchemeObject* bytevector, SchemeObject* k) {
    assert_arg_bytevector_type(L"bytevector-s8-ref", 1, bytevector);
    assert_arg_int_in_range(L"bytevector-s8-ref", 2, k, 0, bytevector->length-1);
    uint8_t b = bytevector->bytevector[scm2int(k)];
    int64_t r = b > 127 ? b - 256 : b;
    return int2scm(r);
}

SchemeObject* s_bytevector_u8_set_e(Scheme* scheme, SchemeObject* bytevector, SchemeObject* k, SchemeObject* octet) {
    assert_arg_bytevector_type(L"bytevector-u8-set!", 1, bytevector);
    assert_arg_not_immutable(L"bytevector-u8-set!", 1, bytevector);
    assert_arg_int_in_range(L"bytevector-u8-set!", 2, k, 0, bytevector->length-1);
    assert_arg_int_in_range(L"bytevector-u8-set!", 3, octet, 0, 255);
    bytevector->bytevector[scm2int(k)] = scm2int(octet);
    return S_UNSPECIFIED;
}

SchemeObject* s_bytevector_s8_set_e(Scheme* scheme, SchemeObject* bytevector, SchemeObject* k, SchemeObject* byte) {
    assert_arg_bytevector_type(L"bytevector-s8-set!", 1, bytevector);
    assert_arg_not_immutable(L"bytevector-s8-set!", 1, bytevector);
    assert_arg_int_in_range(L"bytevector-s8-set!", 2, k, 0, bytevector->length-1);
    assert_arg_int_in_range(L"bytevector-s8-set!", 3, byte, -128, 127);
    int64_t f = scm2int(byte);
    uint8_t b = f < 0 ? 256 + f : f;
    bytevector->bytevector[scm2int(k)] = b;
    return S_UNSPECIFIED;
}

SchemeObject* s_bytevector_u16_ref(Scheme* scheme, SchemeObject* bytevector, SchemeObject* s_k, SchemeObject* endianness) {
    assert_arg_bytevector_type(L"bytevector-u16-ref", 1, bytevector);
    assert_arg_int_in_range(L"bytevector-u16-ref", 2, s_k, 0, bytevector->length-2);
    assert_arg_symbol_type(L"bytevector-u16-ref", 3, endianness);
    uint32_t k = scm2int(s_k);
    uint64_t result;
    uint8_t b1 = bytevector->bytevector[k];
    uint8_t b2 = bytevector->bytevector[k+1];
    if (endianness == big_symbol) {
        result = b1 << 8 | b2;
    } else if (endianness == little_symbol){
        result = b2 << 8 | b1;
    } else {
        throw scheme_exception(L"bytevector-u16-ref", L"Unknown endianness");
    }
    return uint2scm(result);
}

SchemeObject* s_bytevector_s16_ref(Scheme* scheme, SchemeObject* bytevector, SchemeObject* s_k, SchemeObject* endianness) {
    assert_arg_bytevector_type(L"bytevector-s16-ref", 1, bytevector);
    assert_arg_int_in_range(L"bytevector-s16-ref", 2, s_k, 0, bytevector->length-2);
    assert_arg_symbol_type(L"bytevector-s16-ref", 3, endianness);
    uint32_t k = scm2int(s_k);
    int64_t result;
    uint8_t b1 = bytevector->bytevector[k];
    uint8_t b2 = bytevector->bytevector[k+1];
    if (endianness == big_symbol) {
        result = b1 > 127 ? b1 - 256 : b1;
        result = result << 8 | b2;
    } else if (endianness == little_symbol){
        result = b2 > 127 ? b2 - 256 : b2;
        result = result << 8 | b1;
    } else {
        throw scheme_exception(L"bytevector-s16-ref", L"Unknown endianness");
    }
    return int2scm(result);
}

SchemeObject* s_bytevector_u16_native_ref(Scheme* scheme, SchemeObject* bytevector, SchemeObject* s_k) {
    assert_arg_bytevector_type(L"bytevector-u16-native-ref", 1, bytevector);
    assert_arg_int_in_range(L"bytevector-u16-native-ref", 2, s_k, 0, bytevector->length-2);
    return s_bytevector_u16_ref(scheme, bytevector, s_k, s_native_endianness(scheme));
}

SchemeObject* s_bytevector_s16_native_ref(Scheme* scheme, SchemeObject* bytevector, SchemeObject* s_k) {
    assert_arg_bytevector_type(L"bytevector-s16-native-ref", 1, bytevector);
    assert_arg_int_in_range(L"bytevector-s16-native-ref", 2, s_k, 0, bytevector->length-2);
    return s_bytevector_s16_ref(scheme, bytevector, s_k, s_native_endianness(scheme));
}

SchemeObject* s_u8_list_2_bytevector(Scheme* scheme, SchemeObject* l) {
    if (l == S_EMPTY_LIST) {
        return SchemeObject::createBytevector(NULL, 0);
    }
    assert_arg_pair_type(L"u8-list->bytevector", 1, l);

    SchemeObject* p = l;
    uint32_t outlen = 0;
    while (p != S_EMPTY_LIST) {
        outlen++;
        p = i_cdr(p);
    }
    
    uint8_t* bytes = new uint8_t[outlen];

    p = l;
    int32_t i = 0;
    while (p != S_EMPTY_LIST) {
        SchemeObject* s_f = i_car(p);
        assert_arg_int_in_range(L"u8-list->bytevector (list content)", 1, s_f, -128, 255);
        int64_t f = scm2int(s_f);
        bytes[i++] = f < 0 ? 256 + f : f;
        p = i_cdr(p);
    }
    
    return SchemeObject::createBytevector(bytes, outlen);
}

SchemeObject* s_bytevector_2_u8_list(Scheme* scheme, SchemeObject* bytevector) {
    assert_arg_bytevector_type(L"bytevector->u8-list", 1, bytevector);
    SchemeAppendableList result;
    
    for(uint32_t i = 0; i < bytevector->length; i++) {
        result.add(uint2scm(bytevector->bytevector[i]));
    }
    return result.list;
}

#define BIT8 0x80
#define BIT7 0x40
#define BIT6 0x20
#define BIT5 0x10
#define BIT4 0x08
#define BIT3 0x04
#define BIT2 0x02
#define BIT1 0x01
#define SEVEN_BIT_MASK  0x7f  // 01111111
#define SIX_BIT_MASK    0x3f  // 00111111
#define FIVE_BIT_MASK   0x1f  // 00011111
#define FOUR_BIT_MASK   0x0f  // 00001111
#define THREE_BIT_MASK  0x07  // 00000111
#define TWO_BIT_MASK    0x03  // 00000011
#define ONE_BIT_MASK    0x01  // 00000001

SchemeObject* s_string_2_utf8(Scheme* scheme, SchemeObject* str) {
    assert_arg_string_type(L"string->utf8", 1, str);
    uint32_t outlen = 0;
    for(uint32_t i = 0; i < str->length; i++) {
        wchar_t c = str->str[i];
        if (c < 0x80) {
            outlen += 1;
        } else if (c < 0x800) {
            outlen += 2;
        } else if (c < 0x10000) {
            outlen += 3;
        } else {
            outlen += 4;
        }
    }
    
    uint8_t* bytes = new uint8_t[outlen];
    
    uint32_t j = 0;
    for(uint32_t i = 0; i < str->length; i++) {
        wchar_t c = str->str[i];
        if (c < 0x80) {
            bytes[j++] = c & SEVEN_BIT_MASK;
        } else if (c < 0x800) {
            bytes[j++] = BIT8 | BIT7 | ((c >> 6) & FIVE_BIT_MASK);
            bytes[j++] = BIT8 | (c & SIX_BIT_MASK);
        } else if (c < 0x10000) {
            bytes[j++] = BIT8 | BIT7 | BIT6 | ((c >> 12) & FOUR_BIT_MASK);
            bytes[j++] = BIT8 | ((c >> 6) & SIX_BIT_MASK);
            bytes[j++] = BIT8 | (c & SIX_BIT_MASK);
        } else {
            bytes[j++] = BIT8 | BIT7 | BIT6 | BIT5 | ((c >> 18) & THREE_BIT_MASK);
            bytes[j++] = BIT8 | ((c >> 12) & SIX_BIT_MASK);
            bytes[j++] = BIT8 | ((c >> 6) & SIX_BIT_MASK);
            bytes[j++] = BIT8 | (c & SIX_BIT_MASK);
        }
    }
    return SchemeObject::createBytevector(bytes, outlen);
}

SchemeObject* s_utf8_2_string(Scheme* scheme, SchemeObject* bytevector) {
    assert_arg_bytevector_type(L"utf8->string", 1, bytevector);
    uint32_t outlen = 0;
    for(uint32_t i = 0; i < bytevector->length; ) {
        uint8_t b = bytevector->bytevector[i];
        if (b & BIT8) {
            if (b & BIT7) {
                if (b & BIT6) {
                    if (b & BIT5) {
                        outlen++;
                        i += 4;
                    } else {
                        outlen++;
                        i += 3;
                    }
                } else {
                    outlen++;
                    i += 2;
                }
            } else {
                throw scheme_exception(L"utf8->string", L"Invalid UTF-8 sequence");
            }
        } else {
            outlen++;
            i++;
        }
    }
    
    wchar_t* chars = new wchar_t[outlen+1];
    chars[outlen] = '\0';
    
    uint32_t j = 0;
    for(uint32_t i = 0; i < outlen; i++) {
        uint8_t b = bytevector->bytevector[j++];
        uint32_t c;
        if (b & BIT8) {
            if (b & BIT7) {
                if (b & BIT6) {
                    if (b & BIT5) {
                        c = (b & THREE_BIT_MASK) << 18;
                        c |= (bytevector->bytevector[j++] & SIX_BIT_MASK) << 12;
                        c |= (bytevector->bytevector[j++] & SIX_BIT_MASK) << 6;
                        c |= (bytevector->bytevector[j++] & SIX_BIT_MASK); 
                    } else {
                        c = (b & FOUR_BIT_MASK) << 12;
                        c |= (bytevector->bytevector[j++] & SIX_BIT_MASK) << 6;
                        c |= (bytevector->bytevector[j++] & SIX_BIT_MASK); 
                    }
                } else {
                    c = (b & FIVE_BIT_MASK) << 6;
                    c |= (bytevector->bytevector[j++] & SIX_BIT_MASK); 
                }
            } else {
                throw scheme_exception(L"utf8->string", L"Invalid UTF-8 sequence");
            }
        } else {
            c = b;
        }
        chars[i] = c;
    }
    return SchemeObject::createString(chars);
}

void R6RSLibBytevectors::bind(Scheme* scheme, SchemeObject* envt) {
    scheme->assign(L"native_endianness"     ,0,0,0, (SchemeObject* (*)()) s_native_endianness, envt);
    scheme->assign(L"bytevector?"           ,1,0,0, (SchemeObject* (*)()) s_bytevector_p, envt);
    scheme->assign(L"make-bytevector"       ,1,1,0, (SchemeObject* (*)()) s_make_bytevector, envt);
    scheme->assign(L"bytevector-length"     ,1,0,0, (SchemeObject* (*)()) s_bytevector_length, envt);
    scheme->assign(L"bytevector=?"          ,2,0,0, (SchemeObject* (*)()) s_bytevector_equal_p, envt);
    scheme->assign(L"bytevector-fill!"      ,2,0,0, (SchemeObject* (*)()) s_bytevector_fill_e, envt);
    scheme->assign(L"bytevector-copy!"      ,5,0,0, (SchemeObject* (*)()) s_bytevector_copy_e, envt);
    scheme->assign(L"bytevector-copy"       ,1,0,0, (SchemeObject* (*)()) s_bytevector_copy, envt);
    scheme->assign(L"bytevector-u8-ref"     ,2,0,0, (SchemeObject* (*)()) s_bytevector_u8_ref, envt);
    scheme->assign(L"bytevector-s8-ref"     ,2,0,0, (SchemeObject* (*)()) s_bytevector_s8_ref, envt);
    scheme->assign(L"bytevector-u8-set!"    ,3,0,0, (SchemeObject* (*)()) s_bytevector_u8_set_e, envt);
    scheme->assign(L"bytevector-s8-set!"    ,3,0,0, (SchemeObject* (*)()) s_bytevector_s8_set_e, envt);

    scheme->assign(L"bytevector-u16-ref"    ,3,0,0, (SchemeObject* (*)()) s_bytevector_u16_ref, envt);
    scheme->assign(L"bytevector-s16-ref"    ,3,0,0, (SchemeObject* (*)()) s_bytevector_s16_ref, envt);

    scheme->assign(L"u8-list->bytevector"   ,1,0,0, (SchemeObject* (*)()) s_u8_list_2_bytevector, envt);
    scheme->assign(L"bytevector->u8-list"   ,1,0,0, (SchemeObject* (*)()) s_bytevector_2_u8_list, envt);
    scheme->assign(L"string->utf8"          ,1,0,0, (SchemeObject* (*)()) s_string_2_utf8, envt);
    scheme->assign(L"utf8->string"          ,1,0,0, (SchemeObject* (*)()) s_utf8_2_string, envt);

    little_symbol = SchemeObject::createSymbol(L"little");
    big_symbol = SchemeObject::createSymbol(L"big");
}
