
#include "parser/transformationfactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "objects/sceneobject.h"
#include "math/matrix.h"

SchemeObject* TransformationFactory::rotate(Scheme* scheme, SchemeObject* s_obj, SchemeObject* s_axis, SchemeObject* s_angle) 
{
    wchar_t* usage = L"(rotate object axis angle)";
    Vector axis = scm2vector(s_axis, usage, 2);
    double angle = safe_scm2double(s_angle, 3, usage);
    Matrix matrix = Matrix::matrixRotate(axis,angle);
    return transform(scheme, s_obj, matrix, usage);
}

SchemeObject* TransformationFactory::translate(Scheme* scheme, SchemeObject* s_obj, SchemeObject* s_translation) 
{
    wchar_t* usage = L"(translate object translation-vector)";
    Vector translation = scm2vector(s_translation, usage, 2);
    Matrix matrix = Matrix::matrixTranslate(translation);
    return transform(scheme, s_obj, matrix, usage);
}

SchemeObject* TransformationFactory::scale(Scheme* scheme, SchemeObject* s_obj, SchemeObject* s_scale) 
{
    wchar_t* usage = L"(scale object scale-vector)";
    Vector scale = scm2vector(s_scale, usage, 2);
    Matrix matrix = Matrix::matrixScale(scale);
    return transform(scheme, s_obj, matrix, usage);
}

SchemeObject* TransformationFactory::orient(Scheme* scheme, SchemeObject* s_obj, SchemeObject* s_direction_x, SchemeObject* s_direction_y, SchemeObject* s_direction_z) 
{
    wchar_t* usage = L"(orient object orientation-vector-x orientation-vector-y orientation-vector-z) or (orient object direction-vector up-vector) or (orient direction)";
    Vector xv = scm2vector(s_direction_x, usage, 2);
    Vector zv;
    Matrix matrix;
    if (s_direction_y == S_UNSPECIFIED) {
        matrix = Matrix::matrixOrient(xv);
    } else {
        Vector yv = scm2vector(s_direction_y, usage, 3);
	if (s_direction_z == S_UNSPECIFIED) {
            matrix = Matrix::matrixOrient(xv,yv);
        } else {
	    zv = scm2vector(s_direction_z, usage, 4);
            matrix = Matrix::matrixOrient(xv,yv,zv);
	}
    }
    return transform(scheme, s_obj, matrix, usage);
}

/**
 * Transforms a scene object, a vector or a list of sceneobjects.
 */
SchemeObject* TransformationFactory::transform(Scheme* scheme, SchemeObject* s_obj, const Matrix& m, const wchar_t* subr) 
{
    // Tjek if it's a vector
    if (scm2bool(s_vector_p(scheme, s_obj))) {
	if (3 == safe_scm2int(s_vector_length(scheme, s_obj), 0, L"")) {
	    bool is_num = true;
	    for(uint32_t i = 0; i < 3; i++) {
		SchemeObject* thing = s_vector_ref(scheme, s_obj, int2scm(i));
		is_num &= scm2bool(i_number_p(thing));
	    }
	    if (is_num) {
		Vector v = scm2vector(s_obj, subr, 0);
		return vector2scm(m * v);
	    }
	}
    }

    // Check if it's a 4x4 vector Scheme matrix
    if (scm2bool(s_vector_p(scheme, s_obj))) {
	if (4 == i_vector_length(s_obj)) {
	    bool is_good = true;
	    for(uint32_t i = 0; i < 4; i++) {
		SchemeObject* thing = i_vector_ref(s_obj, i);
		if (scm2bool(i_vector_p(thing)) && 
		    (4 == i_vector_length(thing))) {
		    for(uint32_t i = 0; i < 4; i++) {
			SchemeObject* cell = i_vector_ref(thing, i);
			is_good &= scm2bool(i_number_p(cell));
		    }
		} else {
		    is_good = false;
		}
	    }

	    if (is_good) {
		// Extract matrix
		Matrix sm;  
		SchemeObject* row;
	    	for(uint32_t i = 0; i < 4; i++) {
		    row = i_vector_ref(s_obj, i);
		    for(uint32_t c = 0; c < 4; c++) {
			SchemeObject* s_value = i_vector_ref(row,c);
			sm.set(c,i,scm2double(s_value));
		    }
		}
		// Transform
		sm = m * sm;
		// Convert back to a Scheme 4x4 vector
		SchemeObject* result = SchemeObject::createVector(S_FALSE,4);
	    	for(uint32_t i = 0; i < 4; i++) {
		    row = SchemeObject::createVector(S_FALSE,4);
		    for(uint32_t c = 0; c < 4; c++) {
			SchemeObject* v = double2scm(sm.get(i,c));
			i_vector_set_e(row,c,v);
		    }
		    i_vector_set_e(result,i,row);
		}
		return result;
	    }
	}
    }

    
    vector<SchemeObject*> objs;
    if (scm2bool(s_list_p(scheme, s_obj))) {
    	uint32_t num = safe_scm2int(s_length(scheme, s_obj), 0, L"");
    	for(uint32_t i = 0; i < num; i++) {
    	    SchemeObject* s_value = s_list_ref(scheme, s_obj, int2scm(i));
    	    if (scm2bool(s_list_p(scheme, s_value))) {
    	        // Recurse into embedded lists of objects    
    	        transform(scheme, s_value, m, subr);    
    	    } else {
    	        objs.push_back(s_value);
            }
    	}
    } else {
    	objs.push_back(s_obj);
    }

    uint32_t num = objs.size();
    for(uint32_t i = 0; i < num; i++) {
	SceneObject* object = scm2sceneobject(objs[i], subr, i + 1);
	object->transform(m);
    }
    return s_obj;
}

void TransformationFactory::register_procs(Scheme* scheme) 
{
    scheme->assign(L"rotate",3,0,0,(SchemeObject* (*)()) TransformationFactory::rotate);
    scheme->assign(L"translate",2,0,0,(SchemeObject* (*)()) TransformationFactory::translate);
    scheme->assign(L"orient",2,2,0,(SchemeObject* (*)()) TransformationFactory::orient);
    scheme->assign(L"scale",2,0,0,(SchemeObject* (*)()) TransformationFactory::scale);
}
