
#include "parser/converters.h"

// TODO: Optimize the hell out of this
Vector scm2vector(SchemeObject* s_vector, char* subr, int pos) {
    if (!((scm2bool(s_vector_p (s_vector))) &&
    (scm2int(s_vector_length(s_vector), pos, subr) == 3))) {
	wrong_type_arg(subr,pos,s_vector);
    }

    Vector result;
    SchemeObject* s_value;
    for(uint32_t i = 0; i < 3; i++) {
	s_value = s_vector_ref(s_vector, int2scm(i));
	result[i] = safe_scm2double(s_value, pos, subr);
    }
    return result;
}

Quaternion scm2quaternion(SchemeObject* s_vector, char* subr, int pos) {
    if (!((scm2bool(s_vector_p (s_vector))) &&
    (scm2int(s_vector_length(s_vector), pos, subr) == 4))) {
	wrong_type_arg(subr,pos,s_vector);
    }

    double r[4];
    SchemeObject* s_value;
    for(uint32_t i = 0; i < 3; i++) {
	s_value = s_vector_ref(s_vector, int2scm(i));
	r[i] = safe_scm2double(s_value, pos, subr);
    }
    return Quaternion(r[0], r[1], r[2], r[3]);
}

RGBA scm2rgba(SchemeObject* s_vector, char* subr, int pos) {
    if (!((scm2bool(s_vector_p (s_vector))))) {
	wrong_type_arg(subr,pos,s_vector);
    };
    int l = scm2int(s_vector_length(s_vector), pos, subr);
    if (l == 3) {
	return RGBA(scm2rgb(s_vector));
    }
    if (l != 4) {
	wrong_type_arg(subr,pos,s_vector);
    }

    double r[4];
    SchemeObject* s_value;
    for(uint32_t i = 0; i < 4; i++) {
	s_value = s_vector_ref(s_vector, int2scm(i));
	r[i] = safe_scm2double(s_value, pos, subr);
    }
    return RGBA(r[0],r[1],r[2],r[3]);
}

// TODO: Optimize the hell out of this
Vector2 scm2vector2(SchemeObject* s_vector, char* subr, int pos) {
    if (!((scm2bool(s_vector_p (s_vector))) &&
    (scm2int(s_vector_length(s_vector), pos, subr) == 2))) {
	wrong_type_arg(subr,pos,s_vector);
    }

    double res[2];
    SchemeObject* s_value;
    for(uint32_t i = 0; i < 2; i++) {
	s_value = s_vector_ref(s_vector, int2scm(i));
	res[i] = safe_scm2double(s_value, pos, subr);
    }
    return Vector2(res[0], res[1]);
}

SchemeObject* vector2scm(Vector vector) {
    SchemeObject* v0 = s_double2scm(vector[0]);
    SchemeObject* v1 = s_double2scm(vector[1]);
    SchemeObject* v2 = s_double2scm(vector[2]);
    return s_vector(i_list_3(v0,v1,v2));    // TODO: i_list_3 slow!
}

RGB scm2rgb(SchemeObject* s_rgb) {
    Vector v = scm2vector(s_rgb, "unknown", 0);
    return RGB(v[0],v[1],v[2]);
}

SchemeObject* rgb2scm(RGB rgb) {
    SchemeObject* r = s_double2scm(rgb.r());
    SchemeObject* g = s_double2scm(rgb.g());
    SchemeObject* b = s_double2scm(rgb.b());
    return s_vector(i_list_3(r,g,b));
}

vector<Vector> scm2vectorlist(SchemeObject* s_list_vector, char* subr, int pos) {
    assert(scm2bool(s_list_p (s_list_vector)));
    uint32_t length = scm2int(s_length(s_list_vector),0,NULL);
    SchemeObject* s_vector;
    vector<Vector> result;
    for(uint32_t i = 0; i < length; i++) {
	s_vector = s_list_ref(s_list_vector,int2scm(i));
	result.push_back(scm2vector(s_vector, subr, pos));
    }
    return result;
}


double safe_scm2double(SchemeObject* o, int argnum, const char* procname) {
    assert_arg_number_type(procname, argnum, o);        
    return scm2double(o);
}

int safe_scm2int(SchemeObject* o, int argnum, const char* procname) {
    assert_arg_int_type(procname, argnum, o);        
    return scm2int(o);
}
