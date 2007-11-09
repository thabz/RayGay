
// Unicode 5.0 datafiles are at http://www.unicode.org/Public/5.0.0/ucd/

#include "r6rs-lib-unicode.h"

enum category_ids {
    Lu, Ll, Lt, Lm, Lo, Mn, Mc, Me, 
    Nd, Nl, No, Ps, Pe, Pi, Pf, Pd, 
    Pc, Po, Sc, Sm, Sk, So, Zs, Zp, 
    Zl, Cc, Cf, Cs, Co, Cn, LastCatId
};

#include "r6rs-lib-unicode-categories.h"

const static char* category_names[] = {
    "Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Mc", "Me", "Nd", "Nl", "No", 
    "Ps", "Pe", "Pi", "Pf", "Pd", "Pc", "Po", "Sc", "Sm", "Sk", "So", "Zs", "Zp", "Zl", "Cc", 
    "Cf", "Cs", "Co", "Cn"        
};

SchemeObject* category_symbols[LastCatId];

// Returns a symbol representing the Unicode general category 
// of char, one of Lu, Ll, Lt, Lm, Lo, Mn, Mc, Me, Nd, Nl, No, 
// Ps, Pe, Pi, Pf, Pd, Pc, Po, Sc, Sm, Sk, So, Zs, Zp, Zl, Cc, 
// Cf, Cs, Co, or Cn.
SchemeObject* s_char_general_category(SchemeObject* c) {

}

void R6RSLibUnicode::bind(Scheme* scheme, SchemeObject* envt) {
    for(int i = 0; i < LastCatId; i++ ) {
        //category_symbols[i] = SchemeObject::createSymbol(category_names[i]);
    }
    scheme->assign(L"char-general-category"         ,1,0,0, (SchemeObject* (*)()) s_char_general_category, envt);
}
