
#include "parser/converters.h"

Vector scm2vector(SCM s_vector, char* subr, int pos) {
    if (!((SCM_NFALSEP (scm_list_p (s_vector))) &&
    (scm_num2int(scm_length(s_vector), pos, subr) == 3))) {
	scm_wrong_type_arg(subr,pos,s_vector);
    }

    Vector result;
    SCM s_value;
    for(uint i = 0; i < 3; i++) {
	s_value = scm_list_ref(s_vector, scm_int2num(i));
	result[i] = scm_num2double(s_value, pos, subr);
    }
    return result;
}

Vector2 scm2vector2(SCM s_vector, char* subr, int pos) {
    if (!((SCM_NFALSEP (scm_list_p (s_vector))) &&
    (scm_num2int(scm_length(s_vector), pos, subr) == 2))) {
	scm_wrong_type_arg(subr,pos,s_vector);
    }

    double res[2];
    SCM s_value;
    for(uint i = 0; i < 2; i++) {
	s_value = scm_list_ref(s_vector, scm_int2num(i));
	res[i] = scm_num2double(s_value, pos, subr);
    }
    return Vector2(res[0], res[1]);
}

SCM vector2scm(Vector vector) {
    SCM v0 = scm_double2num(vector[0]);
    SCM v1 = scm_double2num(vector[1]);
    SCM v2 = scm_double2num(vector[2]);
    return scm_list_3(v0,v1,v2);
}

RGB scm2rgb(SCM s_rgb) {
    Vector v = scm2vector(s_rgb, "unknown", 0);
    return RGB(v[0],v[1],v[2]);
}

SCM rgb2scm(RGB rgb) {
    SCM r = scm_double2num(rgb.r());
    SCM g = scm_double2num(rgb.g());
    SCM b = scm_double2num(rgb.b());
    return scm_list_3(r,g,b);
}

vector<Vector> scm2vectorlist(SCM s_vector_vector) {
    assert(SCM_NFALSEP (scm_list_p (s_vector_vector)));
    uint length = scm_num2int(scm_length(s_vector_vector),0,"");
    SCM s_vector;
    vector<Vector> result;
    for(uint i = 0; i < length; i++) {
	s_vector = scm_list_ref(s_vector_vector,scm_int2num(i));
	result.push_back(scm2vector(s_vector, "unknown", 0));
    }
    return result;
}

string scm2string(SCM s_string) {
    assert(SCM_STRINGP(s_string));
    size_t* length = NULL;
    char* c_str = gh_scm2newstr(s_string, length);
    return string(c_str);
}
