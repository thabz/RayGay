
#include "parser/transformationfactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "objects/sceneobject.h"
#include "math/matrix.h"

SchemeObject* TransformationFactory::rotate(SchemeObject* s_obj, SchemeObject* s_axis, SchemeObject* s_angle) 
{
    Vector axis = scm2vector(s_axis, "rotate", 2);
    double angle = safe_scm2double(s_angle,3,"rotate");
    Matrix matrix = Matrix::matrixRotate(axis,angle);
    return transform(s_obj, matrix, "rotate");
}

SchemeObject* TransformationFactory::translate(SchemeObject* s_obj, SchemeObject* s_translation) 
{
    Vector translation = scm2vector(s_translation, "translate", 2);
    Matrix matrix = Matrix::matrixTranslate(translation);
    return transform(s_obj, matrix, "translate");
}

SchemeObject* TransformationFactory::scale(SchemeObject* s_obj, SchemeObject* s_scale) 
{
    Vector scale = scm2vector(s_scale, "scale", 2);
    Matrix matrix = Matrix::matrixScale(scale);
    return transform(s_obj, matrix, "scale");
}

/**
 * Transforms a scene object, a vector or a list of sceneobjects.
 */
SchemeObject* TransformationFactory::transform(SchemeObject* s_obj, const Matrix& m, char* subr) 
{
    // Tjek if it's a vector
    if (scm2bool(s_vector_p(s_obj))) {
	if (3 == scm2int(s_vector_length(s_obj),0,"")) {
	    bool is_num = true;
	    for(uint32_t i = 0; i < 3; i++) {
		SchemeObject* thing = s_vector_ref(s_obj, int2scm(i));
		is_num &= scm2bool(i_number_p(thing));
	    }
	    if (is_num) {
		Vector v = scm2vector(s_obj, subr, 0);
		return vector2scm(m * v);
	    }
	}
    }
    
    vector<SchemeObject*> objs;
    if (scm2bool(s_list_p(s_obj))) {
	uint32_t num = scm2int(s_length(s_obj),0,"");
	for(uint32_t i = 0; i < num; i++) {
	    SchemeObject* s_value = s_list_ref(s_obj, int2scm(i));
	    if (scm2bool(s_list_p(s_value))) {
	        // Recurse into embedded lists of objects    
	        transform(s_value, m, subr);    
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
    scheme->assign("rotate",3,0,0,(SchemeObject* (*)()) TransformationFactory::rotate);
    scheme->assign("translate",2,0,0,(SchemeObject* (*)()) TransformationFactory::translate);
    scheme->assign("scale",2,0,0,(SchemeObject* (*)()) TransformationFactory::scale);
}
