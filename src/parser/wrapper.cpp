
#include "parser/wrapper.h"

#include <iostream>

using namespace std;

SCM mark_wrapper (SCM image_smob) {
    cout << "mark_wrapper() called." << endl;
    return SCM_BOOL_F;
}

size_t free_wrapper(SCM image_smob) {
    cout << "free_wrapper() called." << endl;
    return 0;
}

int print_wrapper(SCM image_smob, SCM port, scm_print_state *pstate) {
    cout << "print_wrapper() called." << endl;
    return 1;
}

void init_wrapper_type() {
    wrapped_object_tag = scm_make_smob_type("wrappedobject", sizeof(struct wrapped_object));
    scm_set_smob_mark(wrapped_object_tag, mark_wrapper);
    scm_set_smob_free(wrapped_object_tag, free_wrapper);
    scm_set_smob_print(wrapped_object_tag, print_wrapper);
}
