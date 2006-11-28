
#include "parser/converters.h"

Vector scm2vector(SCM s_vector, char* subr, int pos) {
    if (!((SCM_NFALSEP (scm_vector_p (s_vector))) &&
    (scm_num2int(scm_vector_length(s_vector), pos, subr) == 3))) {
	scm_wrong_type_arg(subr,pos,s_vector);
    }

    Vector result;
    SCM s_value;
    for(uint32_t i = 0; i < 3; i++) {
	s_value = scm_vector_ref(s_vector, scm_int2num(i));
	result[i] = scm_num2double(s_value, pos, subr);
    }
    return result;
}

Quaternion scm2quaternion(SCM s_vector, char* subr, int pos) {
    if (!((SCM_NFALSEP (scm_vector_p (s_vector))) &&
    (scm_num2int(scm_vector_length(s_vector), pos, subr) == 4))) {
	scm_wrong_type_arg(subr,pos,s_vector);
    }

    double r[4];
    SCM s_value;
    for(uint32_t i = 0; i < 3; i++) {
	s_value = scm_vector_ref(s_vector, scm_int2num(i));
	r[i] = scm_num2double(s_value, pos, subr);
    }
    return Quaternion(r[0], r[1], r[2], r[3]);
}

RGBA scm2rgba(SCM s_vector, char* subr, int pos) {
    if (!((SCM_NFALSEP (scm_vector_p (s_vector))))) {
	scm_wrong_type_arg(subr,pos,s_vector);
    };
    int l = scm_num2int(scm_vector_length(s_vector), pos, subr);
    if (l == 3) {
	return RGBA(scm2rgb(s_vector));
    }
    if (l != 4) {
	scm_wrong_type_arg(subr,pos,s_vector);
    }

    double r[4];
    SCM s_value;
    for(uint32_t i = 0; i < 4; i++) {
	s_value = scm_vector_ref(s_vector, scm_int2num(i));
	r[i] = scm_num2double(s_value, pos, subr);
    }
    return RGBA(r[0],r[1],r[2],r[3]);
}

Vector2 scm2vector2(SCM s_vector, char* subr, int pos) {
    if (!((SCM_NFALSEP (scm_vector_p (s_vector))) &&
    (scm_num2int(scm_vector_length(s_vector), pos, subr) == 2))) {
	scm_wrong_type_arg(subr,pos,s_vector);
    }

    double res[2];
    SCM s_value;
    for(uint32_t i = 0; i < 2; i++) {
	s_value = scm_vector_ref(s_vector, scm_int2num(i));
	res[i] = scm_num2double(s_value, pos, subr);
    }
    return Vector2(res[0], res[1]);
}

SCM vector2scm(Vector vector) {
    SCM v0 = scm_double2num(vector[0]);
    SCM v1 = scm_double2num(vector[1]);
    SCM v2 = scm_double2num(vector[2]);
    return scm_vector(scm_list_3(v0,v1,v2));
}

RGB scm2rgb(SCM s_rgb) {
    Vector v = scm2vector(s_rgb, "unknown", 0);
    return RGB(v[0],v[1],v[2]);
}

SCM rgb2scm(RGB rgb) {
    SCM r = scm_double2num(rgb.r());
    SCM g = scm_double2num(rgb.g());
    SCM b = scm_double2num(rgb.b());
    return scm_vector(scm_list_3(r,g,b));
}

vector<Vector> scm2vectorlist(SCM s_list_vector, char* subr, int pos) {
    assert(SCM_NFALSEP (scm_list_p (s_list_vector)));
    uint32_t length = scm_num2int(scm_length(s_list_vector),0,NULL);
    SCM s_vector;
    vector<Vector> result;
    for(uint32_t i = 0; i < length; i++) {
	s_vector = scm_list_ref(s_list_vector,scm_int2num(i));
	result.push_back(scm2vector(s_vector, subr, pos));
    }
    return result;
}

string scm2string(SCM s_string) {
    assert(SCM_STRINGP(s_string));
    size_t* length = NULL;
    char* c_str = gh_scm2newstr(s_string, length);
    return string(c_str);
}
