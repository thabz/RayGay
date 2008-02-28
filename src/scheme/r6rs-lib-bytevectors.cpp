#include "r6rs-lib-bytevectors.h"
#include "scheme.h"
#include "numbers.h"

SchemeObject* s_bytevector_p(SchemeObject* o) {
    return bool2scm(i_bytevector_p(o));
}

SchemeObject* s_make_bytevector(SchemeObject* s_k, SchemeObject* s_fill) {
    assert_arg_positive_int(L"make-bytevector",1, s_k);
    uint64_t k = scm2int(s_k);
    uint8_t* bytes = new uint8_t[k];
    
    if (s_fill != S_UNSPECIFIED) {
        assert_arg_int_in_range(L"make-bytevector", 2, s_fill, -128, 255);
        int64_t f = scm2int(s_fill);
        uint8_t fill = f < 0 ? 256 + f : f;
        for(uint64_t i = 0; i < k; i++) {
            bytes[i] = fill;
        }
    }
    
    return SchemeObject::createBytevector(bytes, k);
}

SchemeObject* s_bytevector_length(SchemeObject* bytevector) {
    assert_arg_bytevector_type(L"bytevector-length", 1, bytevector);
    return int2scm(bytevector->length);
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

SchemeObject* s_string_2_utf8(SchemeObject* str) {
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

SchemeObject* s_utf8_2_string(SchemeObject* bytevector) {
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
    scheme->assign(L"bytevector?"           ,1,0,0, (SchemeObject* (*)()) s_bytevector_p, envt);
    scheme->assign(L"bytevector-length"     ,1,0,0, (SchemeObject* (*)()) s_bytevector_length, envt);
    scheme->assign(L"make-bytevector"       ,1,1,0, (SchemeObject* (*)()) s_make_bytevector, envt);
    scheme->assign(L"string->utf8"          ,1,0,0, (SchemeObject* (*)()) s_string_2_utf8, envt);
    scheme->assign(L"utf8->string"          ,1,0,0, (SchemeObject* (*)()) s_utf8_2_string, envt);
}
