
#include "r6rs-lib-records.h"


// ----------------------------------------------------------
// Procedural layer 
// ----------------------------------------------------------

SchemeObject* s_make_record_type_descriptor(Scheme* scheme, SchemeObject* name, SchemeObject* parent, SchemeObject* uid, SchemeObject* sealed_p, SchemeObject* opaque_p, SchemeObject* fields) {
    return S_FALSE;
}

// ----------------------------------------------------------
// Inspection
// ----------------------------------------------------------

SchemeObject* s_record_p(Scheme* scheme, SchemeObject* obj) {
    return S_FALSE;
}

void R6RSLibRecords::bind(Scheme* scheme, SchemeObject* envt) {
    // Procedural 
    scheme->assign(L"make-record-type-descriptor"     ,1,0,0, (SchemeObject* (*)()) s_make_record_type_descriptor, envt);
    scheme->assign(L"record-type-descriptor?"     ,1,0,0, (SchemeObject* (*)()) s_make_record_type_descriptor, envt);
    scheme->assign(L"make-record-constructor-descriptor"     ,1,0,0, (SchemeObject* (*)()) s_make_record_constructor_descriptor, envt);
    scheme->assign(L"record-constructor"     ,1,0,0, (SchemeObject* (*)()) s_make_record_constructor_descriptor, envt);
    scheme->assign(L"record-predicate"     ,1,0,0, (SchemeObject* (*)()) s_make_record_constructor_descriptor, envt);
    scheme->assign(L"record-accessor"     ,1,0,0, (SchemeObject* (*)()) s_make_record_constructor_descriptor, envt);
    scheme->assign(L"record-mutator"     ,1,0,0, (SchemeObject* (*)()) s_make_record_constructor_descriptor, envt);
    // Inspection
    scheme->assign(L"record?"     ,1,0,0, (SchemeObject* (*)()) s_record_p, envt);
}

